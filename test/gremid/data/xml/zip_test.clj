(ns gremid.data.xml.zip-test
  (:require [gremid.data.xml.zip :as xml.zip]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [gremid.data.xml :as dx]
            [clojure.zip :as zip]))

(def goethe-faust
  (with-open [input (io/input-stream (io/resource "dta_goethe_faust01_1808.xml"))]
    (zip/xml-zip (dx/pull-all (dx/parse input)))))

(dx/alias-uri :xtei "http://www.tei-c.org/ns/1.0")

(deftest find-pagebreaks
  (is (not-empty (xml.zip/xml-> goethe-faust xml.zip/subtree ::xtei/pb))))

(deftest subtree-traversal
  (is
   (=
    [:a :b :c :d :e :f :g :h :i]
    (->> [:a [:b [:c] [:d] [:e]] [:f [:g [:h]]] [:i]]
         dx/sexp-as-element
         zip/xml-zip
         xml.zip/descendants'
         (into [] (map (comp :tag zip/node)))))))

(deftest text-content-without-comments
  (is
   (=
    "abc"
    (->> [:xml "a" [:-comment {:data "Test"}] "bc"]
         (dx/sexp-as-element)
         (zip/xml-zip)
         (xml.zip/text)))))
