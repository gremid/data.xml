(ns gremid.data.xml.lazy-parse-test
  (:require  [clojure.test :refer [deftest is testing]]
             [clojure.java.io :as io]
             [gremid.data.xml :as dx]))


(def test-document
  (io/resource "dta_goethe_faust01_1808.xml"))

(deftest lazy-parsing
  (testing "Parse trees are lazily evaluated by default"
    (let [lazy-doc (with-open [input (io/input-stream test-document)]
                     (dx/parse input))]
      (is (thrown? Exception (last (tree-seq :tag :content lazy-doc))))))
  (testing "The tree can be eagerly evaluated explicity"
    (let [full-doc (with-open [input (io/input-stream test-document)]
                     (dx/pull-all (dx/parse input)))]
      (is (some? (last (tree-seq :tag :content full-doc)))))))
