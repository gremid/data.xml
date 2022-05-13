(ns gremid.data.xml.emit-test
  "Tests for emit to print XML text."
  (:require
   [clojure.walk :refer [postwalk]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.pu-map :as dx.pu]
   [gremid.data.xml.util :refer [cdata document element emit-fragment-str parse-str xml-comment doc-element]]
   [clojure.test :refer [deftest is testing]]))

(def deep-tree
  (parse-str (str "<a h=\"1\" i='2' j=\"3\">"
                  "  t1<b k=\"4\">t2</b>"
                  "  t3<c>t4</c>"
                  "  t5<d>t6</d>"
                  "  t7<e l=\"5\" m=\"6\">"
                  "    t8<f>t10</f>t11</e>"
                  "  t12<g>t13</g>t14"
                  "</a>")))

(deftest test-defaults
  (testing "basic parsing"
    (let [expect (str "<?xml version=\"1.0\"?>"
                      "<a h=\"1\" i=\"2\" j=\"3\">"
                      "  t1<b k=\"4\">t2</b>"
                      "  t3<c>t4</c>"
                      "  t5<d>t6</d>"
                      "  t7<e l=\"5\" m=\"6\">"
                      "    t8<f>t10</f>t11</e>"
                      "  t12<g>t13</g>t14"
                      "</a>")]
      (is (= expect (dx/emit-str deep-tree)))))

  (testing "namespaced defaults"
    (let [expect (str "<?xml version=\"1.0\"?><D:bar xmlns:D=\"DAV:\" D:item=\"1\"><D:baz D:item=\"2\">done</D:baz></D:bar>")]
      (is (= expect (emit-fragment-str
                     (element "{DAV:}bar" {"{DAV:}item" "1" :xmlns/D "DAV:"}
                              (element "{DAV:}baz" {:xmlns.DAV%3A/item "2"} "done")))))
      (is (= expect (emit-fragment-str
                     {:tag "{DAV:}bar" :attrs {"{DAV:}item" "1" :xmlns/D "DAV:"}
                      :content [{:tag "{DAV:}baz" :attrs {:xmlns.DAV%3A/item "2"} :content ["done"]}]}))))))

(deftest mixed-quotes
  (is (= (parse-str
          (str "<?xml version=\"1.0\"?>"
               "<mixed double=\"&quot;double&quot;quotes&quot;here&quot;\""
               " single=\"'single'quotes'here\"></mixed>"))
         (parse-str
          (emit-fragment-str
           (element :mixed
                    {:single "'single'quotes'here"
                     :double "\"double\"quotes\"here\""}))))))

(deftest doctype
  (let [input-tree                    (dx/parse "<how-cool>cool</how-cool>")
        add-dtd                       #(update input-tree :content conj
                                               (dx/sexp-as-element [:-dtd %]))
        doctype-html                  "<!DOCTYPE html>"
        doctype-html-401-transitional "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
        doctype-xhtml-10-strict       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"]
    (is (= (str "<?xml version=\"1.0\"?>"
                doctype-html
                "<how-cool>cool</how-cool>")
           (dx/emit-str (add-dtd doctype-html))))
    (is (= (str "<?xml version=\"1.0\"?>"
                doctype-html-401-transitional
                "<how-cool>cool</how-cool>")
           (dx/emit-str (add-dtd doctype-html-401-transitional))))
    (is (= (str "<?xml version=\"1.0\"?>"
                doctype-xhtml-10-strict
                "<how-cool>cool</how-cool>")
           (dx/emit-str (add-dtd doctype-xhtml-10-strict))))))

(deftest emitting-cdata
  (testing "basic cdata"
    (is
     (= (str "<?xml version=\"1.0\"?>"
             "<cdata-stuff><![CDATA[<goes><here>]]></cdata-stuff>")
        (emit-fragment-str
         (element :cdata-stuff {} (cdata "<goes><here>")))))))

(deftest emitting-comment
  (is
   (= (str "<?xml version=\"1.0\"?>"
           "<comment-stuff>comment <!-- goes here --> not here</comment-stuff>")
      (emit-fragment-str
       (element :comment-stuff {}
                "comment "
                (xml-comment " goes here ")
                " not here")))))

(deftest test-indent
  (let [nested-xml (dx/parse "<a><b><c><d>foo</d></c></b></a>")
        expect     "<a>\n  <b>\n    <c>\n      <d>foo</d>\n    </c>\n  </b>\n</a>\n"
        result     (dx/indent-str nested-xml)]
    (is (= expect (subs result (.indexOf result "<a>"))))))

(deftest test-sibling-xmlns
  (let [el (document
            (element (dx.name/as-qname "{NS1}top") {}
                     (element (dx.name/as-qname "{NS2}foo"))
                     (element (dx.name/as-qname "{NS2}bar"))))]
    (is (= (dx/parse (dx/emit-str el)) el))))

(dx/alias-uri :xml dx.name/xml-uri)

(deftest test-default-xmlns
  (let [nss-meta (comp :gremid.data.xml/nss meta)]
    (is (= (dx.pu/merge-prefix-map nil {"" "NS"})
           (nss-meta (doc-element (dx/parse "<foo xmlns=\"NS\"/>")))
           (nss-meta (doc-element (dx/parse (dx/emit-str (dx/parse "<foo xmlns=\"NS\"/>"))))))))
  (is (thrown? Exception (dx/emit-str {:tag :el :attrs {(dx.name/qname dx.name/xmlns-uri "xml")   "foo"}})))
  (is (thrown? Exception (dx/emit-str {:tag :el :attrs {(dx.name/qname dx.name/xmlns-uri "xmlns") "foo"}})))
  (is (thrown? Exception (dx/emit-str {:tag :el :attrs {:xmlns/xml   "foo"}})))
  (is (thrown? Exception (dx/emit-str {:tag :el :attrs {:xmlns/xmlns "foo"}})))
  (is (thrown? Exception (dx/parse "<element xmlns:xmlns=\"http://www.w3.org/2000/xmlns/\" />"))
      "TODO: find out if this is standard conforming, or a bug in StAX")
  (is (= (emit-fragment-str
          {:tag :el
           :attrs {:xmlns/xmlns "http://www.w3.org/2000/xmlns/"}})
         "<?xml version=\"1.0\"?><el/>"))
  (is (= (emit-fragment-str
          {:tag :el
           :attrs {:xmlns/xml "http://www.w3.org/XML/1998/namespace"
                   ::xml/lang "en"}})
         "<?xml version=\"1.0\"?><el xml:lang=\"en\"/>"))
  (is (= (emit-fragment-str {:tag :el :attrs {:xml/lang "en"}})
         "<?xml version=\"1.0\"?><el xml:lang=\"en\"/>")))

(deftest test-empty-elements
  (is (= (emit-fragment-str {:tag :a :content []})
         "<?xml version=\"1.0\"?><a/>"))
  (is (= (emit-fragment-str {:tag :a :content [""]})
         "<?xml version=\"1.0\"?><a></a>")))

(deftest test-roundtrip
  (let [remove-nss #(postwalk (fn [v] (if (:tag v) (with-meta v nil) v)) %)]
    (is (= (dx/emit-str (remove-nss (dx/parse "<foo:element xmlns:foo=\"FOO:\"/>")))
           "<?xml version=\"1.0\"?><a:element xmlns:a=\"FOO:\"/>")))
  (is (= (dx/emit-str (dx/parse "<foo:element xmlns:xml=\"http://www.w3.org/XML/1998/namespace\" xmlns:foo=\"FOO:\"/>"))
         "<?xml version=\"1.0\"?><foo:element xmlns:foo=\"FOO:\"/>"))
  (is (= (dx/emit-str (dx/parse "<element xmlns=\"FOO:\"/>"))
         "<?xml version=\"1.0\"?><element xmlns=\"FOO:\"/>"))
  ; builtins
  (is (= (dx/emit-str (dx/parse "<element xmlns:xml=\"http://www.w3.org/XML/1998/namespace\" xml:foo=\"FOO!\"/>"))
         "<?xml version=\"1.0\"?><element xml:foo=\"FOO!\"/>"))
  (is (thrown? Exception (dx/parse "<xmlns:el/>"))
      "TODO: find out if this is standard conforming, or a bug in StAX"))
