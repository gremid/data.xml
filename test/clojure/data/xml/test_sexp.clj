(ns clojure.data.xml.test-sexp
  "Tests for reading [:tag {:attr 'value} body*] as XML."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.data.xml :refer [sexp-as-element sexps-as-fragment emit-str]]
   [clojure.data.xml.test-utils :refer [lazy-parse* element cdata xml-comment]]))

(deftest as-element
  (let [xml-input "<tag attr=\"value\"><body /></tag>"
        sexp-input [:tag {:attr "value"} :body]]
    (is (= (lazy-parse* xml-input)
           (sexp-as-element sexp-input)))))

(deftest as-fragment
  (let [input (list [:tag1 "stuff"]
                    [:tag2 "other"])]
    (is (= (sexps-as-fragment input)
           (map sexp-as-element input)))
    (is (thrown? Exception (sexp-as-element input)))))

(deftest with-cdata
  (let [xml-input  (element :tag {:attr "value"}
                            (element :body {}
                                     (cdata "not parsed <stuff")))
        sexp-input [:tag {:attr "value"}
                    [:body {}
                     [:-cdata "not parsed <stuff"]]]]
    (is (= xml-input
           (sexp-as-element sexp-input)))))

(deftest with-multiple-cdata
  (testing "separate cdata"
    (let [xml-input (element :tag {:attr "value"}
                             (element :body {}
                                      (cdata "not parsed <stuff")
                                      (cdata "more not parsed <stuff")))
          sexp-input [:tag {:attr "value"} [:body {}
                                            (list [:-cdata "not parsed <stuff"]
                                                  [:-cdata "more not parsed <stuff"])]]]
      (is (= xml-input
             (sexp-as-element sexp-input)))))
  (testing "cdata with embedded ]]>"
    (let [xml-input (element :tag {:attr "value"}
                             (element :body {}
                                      (cdata "not parsed <stuff]]")
                                      (cdata ">more not parsed <stuff")))
          sexp-input [:tag {:attr "value"}
                      [:body {}
                       [:-cdata "not parsed <stuff]]"]
                       [:-cdata ">more not parsed <stuff"]]]]
      (is (= (emit-str xml-input)
             (emit-str (sexp-as-element sexp-input)))))))

(deftest with-comment
  (let [xml-input (element :tag {:attr "value"}
                           (element :body {} (xml-comment "comment <stuff<here<")))
        sexp-input [:tag {:attr "value"}
                    [:body {}
                     [:-comment "comment <stuff<here<"]]]]
    (is (= xml-input
           (sexp-as-element sexp-input)))))
