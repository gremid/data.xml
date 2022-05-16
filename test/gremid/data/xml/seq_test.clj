(ns gremid.data.xml.seq-test
  "Tests generation of event seqs."
  (:require
   [clojure.test :refer [deftest is]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.util :refer [test-stream]]))

(def str->seq
  (comp dx/->seq test-stream))

(deftest events-with-tags
  (is (every? :tag (str->seq "<?test?><root><a>b</a><c/></root>"))))
