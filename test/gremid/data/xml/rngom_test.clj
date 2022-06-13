(ns gremid.data.xml.rngom-test
  (:require [gremid.data.xml.rngom :as xml.rngom]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]))

(def tei-all
  (xml.rngom/parse-schema (io/resource "tei_all.rng")))

(deftest element-classification
  (let [analysis (xml.rngom/classify-elements tei-all)]
    (is (and (not-empty (:containers analysis))
             (not-empty (:content analysis))))))

(deftest attribute-value-extraction
  (is (not-empty
       (xml.rngom/attribute-values "{http://www.tei-c.org/ns/1.0}space" "dim"
                                   (xml.rngom/traverse tei-all)))))
