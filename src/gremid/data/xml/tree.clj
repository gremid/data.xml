(ns gremid.data.xml.tree
  (:require
   [gremid.data.xml.node :refer [container?]]))

(defn ->node
  [{:keys [tag] :as node}]
  (if (= :-chars tag)
    (-> node :content first)
    (with-meta
      (select-keys node [:tag :attrs :content])
      (reduce dissoc node [:tag :attrs :content]))))

(defn ->trees
  "Returns a lazy sequence whose first element is a sequence of sub-trees and
  whose remaining elements are events that are not siblings or descendants of
  the initial event."
  [vs]
  (lazy-seq
   (when-let [v (first (seq vs))]
     (let [more (rest vs)]
       (if (:gremid.data.xml/end? v)
         (cons nil more)
         (let [node (->node v)
               tree (->trees more)]
           (if (:gremid.data.xml/start? v)
             (let [node    (assoc node :content (lazy-seq (first tree)))
                   subtree (->trees (lazy-seq (rest tree)))]
               (cons (cons node (lazy-seq (first subtree)))
                     (lazy-seq (rest subtree))))
             (cons (cons node (lazy-seq (first tree)))
                   (lazy-seq (rest tree))))))))))

(defn ->tree
  [vs]
  (ffirst (->trees vs)))

(defn ->data
  [node]
  (if (string? node)
    {:tag     :-chars
     :attrs   {}
     :content (list node)}
    (merge node (select-keys (meta node) [:gremid.data.xml/ns-ctx]))))

(defn ->seq
  [node]
  (let [event (->data node)]
    (if (container? node)
      (lazy-cat
       (cons (assoc event :content (list) :gremid.data.xml/start? true)
             (mapcat ->seq (:content event)))
       (list (assoc event :content (list) :gremid.data.xml/end? true)))
      (list event))))
