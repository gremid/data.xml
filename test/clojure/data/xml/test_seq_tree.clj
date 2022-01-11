(ns clojure.data.xml.test-seq-tree
  "Tests for seq-tree, building a lazy tree from lazy seq."
  (:require
   [clojure.data.xml.tree :refer [seq-tree]]
   [clojure.test :refer [deftest is]])
  (:import
   (java.lang.ref WeakReference)))

(def tt
  (partial #'seq-tree #(when (= %1 :<) (vector %2)) #{:>} str))

(deftest example
  (is (= '(("1" "2" [("3" [("4")])] "5") 6)
         (tt [1 2 :< 3 :< 4 :> :> 5 :> 6]))))

(defn limit [& args]
  (tt (concat args (lazy-seq (throw (Exception. "not lazy enough"))))))

(deftest lazy-top-level
  (is (= '()            (take 0 (first (limit 1))))) ; should do better!
  (is (= '("1")         (take 1 (first (limit 1)))))
  (is (= '("1" "2")     (take 2 (first (limit 1 2)))))
  (is (= '("1" "2" "3") (take 3 (first (limit 1 2 3))))))

(deftest lazy-top-level2
  (is (= "1" (reduce nth (limit 1)     [0 0])))
  (is (= "2" (reduce nth (limit 1 2)   [0 1])))
  (is (= "3" (reduce nth (limit 1 2 3) [0 2]))))

(deftest lazy-child
  (is (coll? (reduce nth (limit 1 :<)        [0 1 0])))
  (is (= "2" (reduce nth (limit 1 :< 2)      [0 1 0 0])))
  (is (= "2" (reduce nth (limit 1 :< 2 :>)   [0 1 0 0])))
  (is (= "3" (reduce nth (limit 1 :< 2 :> 3) [0 2]))))

(deftest lazy-end-of-tree
  (is (= 3 (count (first (limit 1 :< 2 :> 3 :>)))))
  (is (= 3 (count (first (limit 1 :< 2 :> 3 :> 4)))))) 

(deftest release-head-top
  (let [input (range 10)
        input-ref (WeakReference. input)
        output (doall (drop 5 (first (tt input))))]
    (System/gc)
    (is (= nil (.get input-ref)))
    output)) 

(deftest release-head-nested-late
  (let [input (list 1 2 :< 3 4 5 :>)
        input-ref (WeakReference. input)
        output (doall (drop 2 (first (tt input))))]
    (System/gc)
    (is (= nil (.get input-ref)))
    output)) 

