(ns gremid.data.xml.pu-map-test
  (:require
   [clojure.test :refer [are deftest is]]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.pu-map :as dx.pu]))

(deftest builtin-mappings
  (is (= dx.name/xml-uri (dx.pu/get dx.pu/EMPTY "xml")))
  (is (= dx.name/xmlns-uri (dx.pu/get dx.pu/EMPTY "xmlns")))
  (is (= ["xml"] (dx.pu/get-prefixes dx.pu/EMPTY dx.name/xml-uri)))
  (is (= ["xmlns"] (dx.pu/get-prefixes dx.pu/EMPTY dx.name/xmlns-uri)))
  (are [p u] (thrown? Exception (dx.pu/assoc dx.pu/EMPTY p u))
    "xml" "_"
    "xmlns" "_"
    "_" dx.name/xml-uri
    "_" dx.name/xmlns-uri))

(deftest basic-operation
  (are [associated-groups expected-uris expected-prefixes]
      (let [pu (reduce (fn [pu* group] (apply dx.pu/assoc pu* group))
                       dx.pu/EMPTY associated-groups)]
        (every? true?
                (apply concat
                       (for [[prefix uri] (partition 2 expected-uris)]
                         (is (= uri (dx.pu/get pu prefix))))
                       (for [[uri prefixes] (partition 2 expected-prefixes)]
                         [(is (= prefixes (dx.pu/get-prefixes pu uri)))
                          (is (= (first prefixes) (dx.pu/get-prefix pu uri)))]))))
      []
      ["wrong-prefix" nil
       "xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri]
      ["wrong-uri" nil
       dx.name/xml-uri ["xml"]
       dx.name/xmlns-uri ["xmlns"]]

      [[nil "FIN:"]]
      ["wrong-prefix" nil
       "xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri
       "" "FIN:"
       nil "FIN:"]
      ["wrong-uri" nil
       "FIN:" [""]
       dx.name/xml-uri ["xml"]
       dx.name/xmlns-uri ["xmlns"]]

      [["p" "U:"
        "q" "V:"]]
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

      [["p" "U:"
        "q" "V:"]
       ["r" "U:"
        "s" "V:"]
       ["t" "U:"]
       ["p" ""
        "q" ""]]
      ["p" nil
       "q" nil
       "r" "U:"]
      ["U:" ["r" "t"]
       "V:" ["s"]]

      [["xml" dx.name/xml-uri
        "xmlns" dx.name/xmlns-uri]]
      ["xml" dx.name/xml-uri
       "xmlns" dx.name/xmlns-uri]
      [dx.name/xml-uri ["xml"]
       dx.name/xmlns-uri ["xmlns"]]))

(deftest assoc-nil
  (let [pu (dx.pu/assoc nil nil "NIL")]
    (is (= "NIL" (dx.pu/get pu nil) (dx.pu/get pu "")))
    (is (= "" (dx.pu/get-prefix pu "NIL")))))

(deftest direct-access
  (is (= {"" "NIL" "a" "A"
          "xml" dx.name/xml-uri
          "xmlns" dx.name/xmlns-uri}
         (dx.pu/prefix-map
          (dx.pu/assoc nil "a" "A" nil "NIL")))))

(deftest diffing
  (is (= {"c" "d"}
         (dx.pu/reduce-diff
          assoc {}
          (dx.pu/assoc dx.pu/EMPTY "a" "b")
          (dx.pu/assoc dx.pu/EMPTY
                    "a" "b" "c" "d")))))
