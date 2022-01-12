(ns gremid.data.xml.parse-test
  "Tests for XML parsing functions."
  (:require
   [clojure.test :refer [deftest is are]]
   [gremid.data.xml.name :refer [qname]]
   [gremid.data.xml :refer [parse-str]]
   [gremid.data.xml.util :refer [lazy-parse* element]]))

(deftest test-node-equivalence
  (are [repr1 repr2] (and (is (= repr1 repr2))
                          (is (= (hash repr1) (hash repr2))))
    (element :foo)                  {:tag :foo :attrs {} :content []}
    (element (qname "DAV:" "foo"))  {:tag (qname "DAV:" "foo") :attrs {} :content []}
    (element :foo {:a "b"})         {:tag :foo :attrs {:a "b"} :content []}
    (element :foo {:a "b"} "a" "b") {:tag :foo :attrs {:a "b"} :content ["a" "b"]}))

(deftest simple
  (let [input "<html><body bg=\"red\">This is <b>bold</b> test</body></html>"
        expected (element :html {} (element :body {:bg "red"}
                   "This is " (element :b {} "bold") " test"))]
    (is (= expected (lazy-parse* input)))))

(deftest deep
  (let [input (str "<a h='1' i=\"2\" j='3'>"
                   "  t1<b k=\"4\">t2</b>"
                   "  t3<c>t4</c>"
                   "  t5<d>t6</d>"
                   "  t7<e l='5' m='6'>"
                   "    t8<f>t10</f>t11</e>"
                   "  t12<g>t13</g>t14"
                   "</a>")
        expected (element :a {:h "1", :i "2", :j "3"}
                   "  t1" (element :b {:k "4"} "t2")
                   "  t3" (element :c {} "t4")
                   "  t5" (element :d {} "t6")
                   "  t7" (element :e {:l "5" :m "6"}
                   "    t8" (element :f {} "t10") "t11")
                   "  t12" (element :g {} "t13") "t14")]
    (is (= expected (lazy-parse* input)))
    (is (= expected (parse-str input)))))

(deftest test-xml-with-whitespace
    (let [input (str "<a>\n<b with-attr=\"s p a c e\">123</b>\n<c>1 2 3</c>\n\n</a>")
          expected (element :a {}
                            "\n"
                            (element :b {:with-attr "s p a c e"} "123")
                            "\n"
                            (element :c {}  "1 2 3")
                            "\n\n")]
    (is (= expected (lazy-parse* input)))))

(deftest test-cdata-parse
(let [input "<cdata><is><here><![CDATA[<dont><parse><me>]]></here></is></cdata>"
      expected (element :cdata {} (element :is {}
                                           (element :here {}
                                                    "<dont><parse><me>")))]
  (is (= expected (lazy-parse* input)))))

(deftest test-comment-parse
(let [input "<comment><is><here><!-- or could be -->there</here></is></comment>"
      expected (element :comment {} (element :is {}
                                           (element :here {}
                                                    "there")))]
  (is (= expected (lazy-parse* input)))))

(deftest test-parsing-processing-instructions
  (let [input "<?xml version=\"1.0\" encoding=\"utf-8\"?>
                <?xml-stylesheet type='text/xsl' href='someFile.xsl'?>
                <ATag>With Stuff</ATag>"
        expected (element :ATag {} "With Stuff")]
    (is (= expected (parse-str input)))))

(deftest test-parsing-doctypes
  (let [input "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
               \"foo://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
               <html><h1>Heading Stuff</h1></html>"
        expected (element :html {}
                          (element :h1 {} "Heading Stuff"))]
    (is (= expected (parse-str input)))))

(deftest test-coalescing
  (let [input "<a><![CDATA[\nfoo bar\n]]><![CDATA[\nbaz\n]]></a>"]
    (is (= ["\nfoo bar\n\nbaz\n"] (:content (parse-str input))))
    (is (= ["\nfoo bar\n" "\nbaz\n"] (:content
                                      (parse-str input :coalescing false))))))

(deftest test-location-meta
  (let [input "<a><b/>\n<b/></a>"
        location-meta (comp :gremid.data.xml/location-info meta)]
    ;the numbers look 1 based
    (is (= 1 (-> input parse-str location-meta :line-number)))
    (is (= 1 (-> input parse-str location-meta :column-number)))
    (is (= 1 (-> input parse-str :content first location-meta :line-number)))
    (is (= 4 (-> input parse-str :content first location-meta :column-number)))
    (is (= 2 (-> input (parse-str :skip-whitespace true) :content second location-meta :line-number)))
    (is (nil? (-> input
                  (parse-str :location-info false)
                  location-meta)))))

(deftest test-ignorable-whitespace
  ;; FIXME implement clojure.lang.MapEquivalence for records
  (clojure.lang.APersistentMap/mapEquals
   (parse-str "<?xml version=\"1.0\"?>
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
