;   Copyright (c) Rich Hickey and contributors. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.process
  (:require
   [clojure.data.xml.name
    :as name
    :refer [*gen-prefix-counter* gen-prefix qname-uri]]
   [clojure.data.xml.pu-map :as pu]))

(defn- reduce-tree
  "Optimized reducer for in-order traversal of nodes, with reduce-like accumulator"
  [f init xml]
  (loop [result init
         {:as tree [child & next-children :as children] :content} xml
         [parent & next-parents :as parents] ()]
    (if (seq children)
      (recur (f result tree)
             child
             (concat next-children parents))
      (if (seq parents)
        (recur (f result tree)
               parent
               next-parents)
        (f result tree)))))

(defn- qname-uri-xf [xf]
  (fn [s el]
    (if (map? el)
      (reduce-kv
       (fn [s attr _] (xf s (qname-uri attr)))
       (xf s (qname-uri (:tag el)))
       (:attrs el))
      s)))

(defn find-xmlns
  "Find all xmlns occuring in a root"
  [xml]
  (persistent!
   (reduce-tree (qname-uri-xf conj!)
                (transient #{}) xml)))

(defn aggregate-xmlns
  "Put all occurring xmlns into the root"
  [xml]
  (with-meta
    xml {:clojure.data.xml/nss
         (binding [*gen-prefix-counter* 0]
           (-> (fn [tm uri]
                 (pu/assoc! tm (gen-prefix) uri))
               qname-uri-xf
               (reduce-tree (pu/transient pu/EMPTY) xml)
               pu/persistent!))}))
