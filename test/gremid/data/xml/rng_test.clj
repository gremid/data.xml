(ns gremid.data.xml.rng-test
  (:require [gremid.data.xml.rng :as xml.rng]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]))

(def schema
  (xml.rng/->schema (io/resource "tei_all.rng")))

(def goethe-faust
  (io/resource "dta_goethe_faust01_1808.xml"))

(deftest validate-goethe-faust
  (is (empty? (xml.rng/validate schema goethe-faust))))

