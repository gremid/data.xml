(ns gremid.data.xml.sexp-test
  "Tests for reading [:tag {:attr 'value} body*] as XML."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.node :refer [cdata element xml-comment]]))

(deftest as-element
  (is (= (dx/parse "<tag attr=\"value\"><body /></tag>")
         (dx/sexp-as-element
          [:-document
           {:encoding nil :standalone nil :system-id nil}
           [:tag {:attr "value"}
            :body]]))))

(deftest as-fragment
  (let [input (list [:tag1 "stuff"]
                    [:tag2 "other"])]
    (is (= (dx/sexps-as-fragment input)
           (map dx/sexp-as-element input)))
    (is (thrown? Exception (dx/sexp-as-element input)))))

(deftest with-cdata
  (is (= (->> (cdata "not parsed <stuff")
              (element :body {})
              (element :tag {:attr "value"}))
         (dx/sexp-as-element
          [:tag {:attr "value"}
           [:body {}
            [:-cdata "not parsed <stuff"]]]))))

(deftest with-multiple-cdata
  (testing "separate cdata"
    (is (= (element
            :tag {:attr "value"}
            (element
             :body {}
             (cdata "not parsed <stuff")
             (cdata "more not parsed <stuff")))
           (dx/sexp-as-element
            [:tag {:attr "value"}
             [:body {}
              (list [:-cdata "not parsed <stuff"]
                    [:-cdata "more not parsed <stuff"])]]))))
  (testing "cdata with embedded ]]>"
    (is (= (dx/emit-str (element
                      :tag {:attr "value"}
                      (element
                       :body {}
                       (cdata "not parsed <stuff]]")
                       (cdata ">more not parsed <stuff"))))
           (dx/emit-str (dx/sexp-as-element
                      [:tag {:attr "value"}
                       [:body {}
                        [:-cdata "not parsed <stuff]]"]
                        [:-cdata ">more not parsed <stuff"]]]))))))

(deftest with-comment
  (is (= (element
          :tag {:attr "value"}
          (element
           :body {}
           (xml-comment "comment <stuff<here<")))
         (dx/sexp-as-element
          [:tag {:attr "value"}
           [:body {}
            [:-comment "comment <stuff<here<"]]]))))
