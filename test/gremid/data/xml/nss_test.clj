(ns gremid.data.xml.nss-test
  (:require
   [clojure.test :refer [are deftest is]]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.nss :as dx.nss]))

(deftest builtin-mappings
  (is (= dx.name/xml-uri (get dx.nss/EMPTY "xml")))
  (is (= dx.name/xmlns-uri (get dx.nss/EMPTY "xmlns")))
  (is (= ["xml"] (dx.nss/get-prefixes dx.nss/EMPTY dx.name/xml-uri)))
  (is (= ["xmlns"] (dx.nss/get-prefixes dx.nss/EMPTY dx.name/xmlns-uri)))
  (are [p u] (thrown? Exception (dx.nss/assoc' dx.nss/EMPTY p u))
    "xml"   "_"
    "xmlns" "_"
    "_"     dx.name/xml-uri
    "_"     dx.name/xmlns-uri))

(deftest basic-operation
  (are [associated-groups expected-uris expected-prefixes]
      (let [pm (reduce
                (fn [pm [prefix uri]] (dx.nss/assoc' pm prefix uri))
                dx.nss/EMPTY
                (partition 2 associated-groups))]
        (every?
         true?
         (apply concat
                (for [[prefix uri] (partition 2 expected-uris)]
                  (is (= uri (get pm prefix))))
                (for [[uri prefixes] (partition 2 expected-prefixes)]
                  [(is (= prefixes (dx.nss/get-prefixes pm uri)))
                   (is (= (first prefixes) (dx.nss/get-prefix pm uri)))]))))
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
         (dx.nss/assoc' dx.nss/EMPTY "a" "A"))))

(deftest diffing
  (is
   (= [["c" "d"]] (vec (dx.nss/diff {"a" "b"} {"a" "b" "c" "d"})))))
