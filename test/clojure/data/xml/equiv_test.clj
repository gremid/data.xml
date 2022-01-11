(ns clojure.data.xml.equiv-test
  (:require
   [clojure.data.xml.name :refer [qname]]
   [clojure.data.xml.util :refer [element]]
   [clojure.test :refer [are deftest is]]))

(deftest test-node-equivalence
  (are [repr1 repr2] (and (is (= repr1 repr2))
                          (is (= (hash repr1) (hash repr2))))
    (element :foo) {:tag :foo :attrs {} :content []}
    (element (qname "DAV:" "foo")) {:tag (qname "DAV:" "foo") :attrs {} :content []}
    (element :foo {:a "b"}) {:tag :foo :attrs {:a "b"} :content []}
    (element :foo {:a "b"} "a" "b") {:tag :foo :attrs {:a "b"} :content ["a" "b"]}))
