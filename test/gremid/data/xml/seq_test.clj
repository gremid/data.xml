(ns gremid.data.xml.seq-test
  "Tests generation of event seqs."
  (:require
   [clojure.test :refer [deftest is]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.util :refer [test-stream]]))

(def str->seq
  (comp dx/->seq test-stream))

(def seq->str
  (comp dx/stream-str (partial remove (comp #{:-document} :tag))))

(deftest events-with-tags
  (is (every? :tag (str->seq "<?test?><root><a>b</a><c/></root>"))))

(deftest roundtripping
  (let [xml "<?test?><root><a>b</a><c/></root>"]
    (is (= xml (-> xml str->seq seq->str)))))

(deftest namespaces
  (let [xml "<?test?><root xmlns=\"urn:test\"><a>b</a><c/></root>"]
    (is (= xml (-> xml str->seq seq->str)))))
