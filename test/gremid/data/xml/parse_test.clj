(ns gremid.data.xml.parse-test
  "Tests for XML parsing functions."
  (:require
   [clojure.test :refer [are deftest is]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.node :refer [cdata dtd element document pi xml-comment doc-element]]))

(deftest test-node-equivalence
  (are [repr1 repr2] (and (is (= repr1 repr2))
                          (is (= (hash repr1) (hash repr2))))
    (element :foo)                         {:tag :foo :attrs {} :content []}
    (element (dx.name/qname "DAV:" "foo")) {:tag     (dx.name/qname "DAV:" "foo")
                                            :attrs   {}
                                            :content []}
    (element :foo {:a "b"})                {:tag :foo :attrs {:a "b"} :content []}
    (element :foo {:a "b"} "a" "b")        {:tag     :foo
                                            :attrs   {:a "b"}
                                            :content ["a" "b"]}))

(deftest simple
  (let [input "<html><body bg=\"red\">This is <b>bold</b> test</body></html>"
        expected (document
                  (element
                   :html {}
                   (element
                    :body {:bg "red"}
                    "This is " (element :b {} "bold") " test")))]
    (is (= expected (dx/parse input)))))

(deftest deep
  (let [input (str "<a h='1' i=\"2\" j='3'>"
                   "  t1<b k=\"4\">t2</b>"
                   "  t3<c>t4</c>"
                   "  t5<d>t6</d>"
                   "  t7<e l='5' m='6'>"
                   "    t8<f>t10</f>t11</e>"
                   "  t12<g>t13</g>t14"
                   "</a>")
        expected (document
                  (element :a {:h "1", :i "2", :j "3"}
                           "  t1" (element :b {:k "4"} "t2")
                           "  t3" (element :c {} "t4")
                           "  t5" (element :d {} "t6")
                           "  t7" (element :e {:l "5" :m "6"}
                                           "    t8" (element :f {} "t10") "t11")
                           "  t12" (element :g {} "t13") "t14"))]
    (is (= expected (dx/parse input)))))

(deftest test-xml-with-whitespace
    (let [input (str "<a>\n<b with-attr=\"s p a c e\">123</b>\n<c>1 2 3</c>\n\n</a>")
          expected (document
                    (element :a {}
                             "\n"
                             (element :b {:with-attr "s p a c e"} "123")
                             "\n"
                             (element :c {}  "1 2 3")
                             "\n\n"))]
    (is (= expected (dx/parse input)))))

(deftest test-cdata-parse
(let [input "<cdata><is><here><![CDATA[<dont><parse><me>]]></here></is></cdata>"
      expected (document
                (element
                 :cdata {}
                 (element
                  :is {}
                  (element
                   :here {}
                   (cdata "<dont><parse><me>")))))]
  (is (= expected (dx/parse input)))))

(deftest test-comment-parse
(let [input "<comment><is><here><!-- or could be -->there</here></is></comment>"
      expected (document
                (element
                 :comment {}
                 (element
                  :is {}
                  (element
                   :here {}
                   (xml-comment " or could be ")
                   "there"))))]
  (is (= expected (dx/parse input)))))

(deftest test-parsing-processing-instructions
  (let [input (str "<?xml version=\"1.0\"?>"
                   "<?xml-stylesheet type='text/xsl' href='someFile.xsl'?>"
                   "<ATag>With Stuff</ATag>")
        expected (document
                  (pi "xml-stylesheet" "type='text/xsl' href='someFile.xsl'")
                  (element :ATag {} "With Stuff"))]
    (is (= expected (dx/parse input)))))

(dx/alias-uri :xhtml "http://www.w3.org/1999/xhtml")

(deftest test-parsing-doctypes
  (let [input "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><h1>Heading Stuff</h1></html>"
        expected (document
                  (dtd "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\" []>")
                  (element
                   ::xhtml/html {}
                   (element ::xhtml/h1 {} "Heading Stuff")))]
    (is (= expected (dx/parse input)))))

(deftest test-location-meta
  (let [input         "<a><b/>\n<b/></a>"
        location-meta (comp :gremid.data.xml/location-info meta)]
    (let [location (-> input dx/parse doc-element location-meta)]
      (is (= {:line-number 1 :column-number 1 :character-offset 0} location)))))

(deftest test-ignorable-whitespace
  ;; FIXME implement clojure.lang.MapEquivalence for records
  (clojure.lang.APersistentMap/mapEquals
   (dx/parse "<?xml version=\"1.0\"?>
<!DOCTYPE methodCall [
  <!ELEMENT methodCall (methodName, params)>
  <!ELEMENT params (param+)>
  <!ELEMENT param (value)>
  <!ELEMENT value (string)>
  <!ELEMENT methodName (#PCDATA)>
  <!ELEMENT string (#PCDATA)>
]>
<methodCall>
  <methodName>lookupSymbol</methodName>
  <params>
    <param>
      <value>
        <string>
          Clojure XML &lt;3
        </string>
      </value>
    </param>
  </params>
</methodCall>")
   {:tag :methodCall, :attrs {}, :content
    [{:tag :methodName, :attrs {}, :content
      ["lookupSymbol"]}
     {:tag :params, :attrs {}, :content
      [{:tag :param, :attrs {}, :content
        [{:tag :value, :attrs {}, :content
          [{:tag :string, :attrs {}, :content
            ["\n          Clojure XML <3 \n        "]}]}]}]}]}))
