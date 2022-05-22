(ns gremid.data.xml.name-test
  (:require
   [clojure.test :refer [are deftest is]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.node :refer [doc-element document element]]
   [gremid.data.xml.test-util :refer [emit-fragment-str]]))

(dx/alias-uri
 :U "uri-u:"
 :D "DAV:"
 'V "uri-v:"
 "W" "uri-w:")

(deftest test-types
  (are [vals values] (every? true? (for [v values]
                                     (is (= vals [(dx.name/qname-uri v)
                                                  (dx.name/qname-local v)])
                                         (str "Interpreted QName: " (pr-str v)))))
    ["" "name"]           ["name" :name (dx.name/parse-qname "name")]
    ["uri-u:" "name"]     [::U/name "{uri-u:}name"
                           (dx.name/parse-qname "{uri-u:}name")
                           (dx.name/as-qname "{uri-u:}name")]
    ["uri-v:" "vname"]    [::V/vname "{uri-v:}vname"
                           (dx.name/parse-qname "{uri-v:}vname")]
    ["uri-w:" "wname"]    [::W/wname "{uri-w:}wname"
                           (dx.name/parse-qname "{uri-w:}wname")]
    ;; ["http://www.w3.org/XML/1998/namespace" "name"] [:xml/name]
    ["http://www.w3.org/2000/xmlns/" "name"]        [:xmlns/name]))


(deftest test-emit-raw
  (are [node result] (= (emit-fragment-str node) result)
    {:tag ::D/limit :attrs {:xmlns/D "DAV:"}
     :content [{:tag ::D/nresults :content ["100"]}]}
    "<?xml version=\"1.0\"?><D:limit xmlns:D=\"DAV:\"><D:nresults>100</D:nresults></D:limit>"))

(deftest test-parse-raw
  (are [xml result] (= result (dx/parse xml))
    "<D:limit xmlns:D=\"DAV:\"><D:nresults>100</D:nresults></D:limit>"
    (document (element ::D/limit {} (element ::D/nresults nil "100")))))

(deftest qnames
  (is (= (dx.name/qname "foo") (dx.name/as-qname :foo))))

(deftest test-gen-prefix
  (are [node] (= node (dx/parse (dx/emit-str node)) )
    (document
     (element ::D/limit {::V/moo "gee"} (element ::D/nresults nil "100")))))

(deftest test-reassign-prefix
  (are [node reparsed] (= reparsed (dx/parse (dx/emit-str node)))
    (document
     (element ::D/limit {:xmlns/D "DAV:"}
              ;; because of outer binding, "uri-v:" will be bound to
              ;; generated xmlns:a instead of xmlns:D
              (element ::V/other {:xmlns/D "uri-v:"})))
    (document
     (element ::D/limit {} (element ::V/other)))))

(deftest test-preserve-empty-ns
  (are [el] (= el (doc-element
                   (dx/parse
                    (emit-fragment-str
                     (assoc-in el [:attrs :xmlns] "DAV:")))))
    (element :top-level)
    (element ::D/local-root {} (element :top-level))))

(deftest builtin-mappings
  (is (= dx.name/xml-uri (get dx.name/initial-ns-ctx "xml")))
  (is (= dx.name/xmlns-uri (get dx.name/initial-ns-ctx "xmlns")))
  (is (= ["xml"] (dx.name/get-prefixes dx.name/initial-ns-ctx dx.name/xml-uri)))
  (is (= ["xmlns"] (dx.name/get-prefixes dx.name/initial-ns-ctx dx.name/xmlns-uri)))
  (are [p u] (thrown? Exception (dx.name/assoc' dx.name/initial-ns-ctx p u))
    "xml"   "_"
    "xmlns" "_"
    "_"     dx.name/xml-uri
    "_"     dx.name/xmlns-uri))

(deftest basic-operation
  (are [associated-groups expected-uris expected-prefixes]
      (let [pm (reduce
                (fn [pm [prefix uri]] (dx.name/assoc' pm prefix uri))
                dx.name/initial-ns-ctx
                (partition 2 associated-groups))]
        (every?
         true?
         (apply concat
                (for [[prefix uri] (partition 2 expected-uris)]
                  (is (= uri (get pm prefix))))
                (for [[uri prefixes] (partition 2 expected-prefixes)]
                  [(is (= prefixes (dx.name/get-prefixes pm uri)))
                   (is (= (first prefixes) (dx.name/get-prefix pm uri)))]))))
      []
      ["wrong-prefix" nil
       "xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri]
      ["wrong-uri" nil
       dx.name/xml-uri ["xml"]
       dx.name/xmlns-uri ["xmlns"]]

      ["p" "U:"
       "q" "V:"]
      ["wrong-prefix" nil
       "xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri
       "p" "U:"
       "q" "V:"]
      ["wrong-uri" nil
       dx.name/xml-uri ["xml"]
       dx.name/xmlns-uri ["xmlns"]
       "U:" ["p"]
       "V:" ["q"]]

      ["p" "U:"
       "q" "V:"
       "r" "U:"
       "s" "V:"
       "t" "U:"
       "p" ""
       "q" ""]
      ["p" nil
       "q" nil
       "r" "U:"]
      ["U:" ["r" "t"]
       "V:" ["s"]]

      ["xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri]
      ["xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri]
      [dx.name/xml-uri ["xml"]
       dx.name/xmlns-uri ["xmlns"]]))

(deftest direct-access
  (is (= {"a"     "A"
          "xml"   dx.name/xml-uri
          "xmlns" dx.name/xmlns-uri}
         (dx.name/assoc' dx.name/initial-ns-ctx "a" "A"))))

(deftest diffing
  (is
   (= [["c" "d"]] (vec (dx.name/diff {"a" "b"} {"a" "b" "c" "d"})))))
