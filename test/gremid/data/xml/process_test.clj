(ns gremid.data.xml.process-test
  (:require
   [gremid.data.xml :refer [element-nss]]
   [gremid.data.xml.name :refer [qname]]
   [gremid.data.xml.process :refer [aggregate-xmlns find-xmlns]]
   [gremid.data.xml.pu-map :as pu]
   [gremid.data.xml.util :refer [element]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [clojure.walk :as w]))

(def test-data
  (element
   :foo nil
   (with-meta (element :bar {:xmlns "MOO:"} "some" "content")
     {:gremid.data.xml/nss (pu/merge-prefix-map nil {"p" "PAR:"})})
   "more content"
   (element (qname "GOO:" "ho") {(qname "GEE:" "hi") "ma"} "ii")
   "end"))

(deftest process
  (is (= (find-xmlns test-data) #{"" "GEE:" "GOO:"}))
  (let [nss (set (vals (:p->u (element-nss (aggregate-xmlns test-data)))))]
    (is (every? #(contains? nss %) ["GEE:" "GOO:"]))))

(deftest walk-test
  (is (= {:tag :FOO, :attrs {}, :content ()}
         (w/postwalk (fn [e]
                       (if (:tag e)
                         (update e :tag (comp keyword str/upper-case name))
                         e))
                     (element :foo)))))
