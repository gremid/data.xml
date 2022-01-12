(ns gremid.data.xml.pi-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [gremid.data.xml :refer [emit-str parse-str]]))

(deftest roundtrip-pis
  (let [doc-str (-> (io/resource "pi-test-doc.xml")
                    (slurp :encoding "UTF-8"))]
    (is (= doc-str
           (-> doc-str
               (parse-str :include-node? #{:pi :element :characters})
               (emit-str))))))
