(ns gremid.data.xml.pi-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [gremid.data.xml :as dx]))

(deftest roundtrip-pis
  (let [doc-str (-> (io/resource "pi-test-doc.xml")
                    (slurp :encoding "UTF-8"))]
    (is (= doc-str (dx/emit-str (dx/parse doc-str))))))
