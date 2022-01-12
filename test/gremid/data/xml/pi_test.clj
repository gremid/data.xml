(ns gremid.data.xml.pi-test
  (:require  [clojure.test :refer [deftest is]]
             [clojure.java.io :as io]
             [gremid.data.xml :refer [emit-str parse-str]]
             [clojure.string :as str]))

;; FIXME: whitespace before and after the root element is not provided by the
;; StAX parser
(deftest roundtrip-pis
  (let [doc-str (-> (io/resource "pi-test-doc.xml")
                    (slurp :encoding "UTF-8")
                    (str/trim))]
    (is (= doc-str
           (-> doc-str
               (parse-str :include-node? #{:pi :element :characters})
               (emit-str))))))
