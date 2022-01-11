(ns clojure.data.xml.tree
  (:require [clojure.data.xml.name :refer [separate-xmlns]]
            [clojure.data.xml.pu-map :as pu]))

(defn seq-tree
  "Takes a seq of events that logically represents
  a tree by each event being one of: enter-sub-tree event,
  exit-sub-tree event, or node event.

  Returns a lazy sequence whose first element is a sequence of
  sub-trees and whose remaining elements are events that are not
  siblings or descendants of the initial event.

  The given exit? function must return true for any exit-sub-tree
  event.  parent must be a function of two arguments: the first is an
  event, the second a sequence of nodes or subtrees that are children
  of the event.  parent must return nil or false if the event is not
  an enter-sub-tree event.  Any other return value will become
  a sub-tree of the output tree and should normally contain in some
  way the children passed as the second arg.  The node function is
  called with a single event arg on every event that is neither parent
  nor exit, and its return value will become a node of the output tree.

  (seq-tree #(when (= %1 :<) (vector %2)) #{:>} str
            [1 2 :< 3 :< 4 :> :> 5 :> 6])
  ;=> ((\"1\" \"2\" [(\"3\" [(\"4\")])] \"5\") 6)"
  [parent exit? node coll]
  (lazy-seq
   (when-let [[event] (seq coll)]
     (let [more (rest coll)]
       (if (exit? event)
         (cons nil more)
         (let [tree (seq-tree parent exit? node more)]
           (if-let [p (parent event (lazy-seq (first tree)))]
             (let [subtree (seq-tree parent exit? node (lazy-seq (rest tree)))]
               (cons (cons p (lazy-seq (first subtree)))
                     (lazy-seq (rest subtree))))
             (cons (cons (node event) (lazy-seq (first tree)))
                   (lazy-seq (rest tree))))))))))

;; "Parse" events off the in-memory representation

(defn element->first-event
  [{:keys [attrs content] :as node}]
  (let [[attrs' xmlns-attrs] (separate-xmlns attrs)]
    (-> node
        (assoc :attrs attrs')
        (vary-meta assoc :clojure.data.xml/event
                   (if (seq content) :start :empty))
        (vary-meta update :clojure.data.xml/nss
                   pu/merge-prefix-map xmlns-attrs))))

(defn node->first-event
  [node]
  (cond
    (string? node)     (with-meta
                         {:content [node]}
                         {:clojure.data.xml/event :chars})
    (sequential? node) (node->first-event (first node))
    (node :tag)        (condp = (node :tag)
                         :-comment (vary-meta node assoc
                                              :clojure.data.xml/event :comment)
                         :-cdata   (vary-meta node assoc
                                              :clojure.data.xml/event :cdata)
                         (element->first-event node))
    :else              node))

(defn element->rest-events
  [{:keys [content]} next-nodes]
  (if (seq content)
    (list* content (with-meta {} {:clojure.data.xml/event :end}) next-nodes)
    next-nodes))

(defn node->rest-events
  [node next-nodes]
  (cond
    (string? node)     next-nodes
    (sequential? node) (if-let [r (seq (rest node))]
                         (cons (node->rest-events (first node) r) next-nodes)
                         (node->rest-events (first node) next-nodes))
    (node :tag)        (condp = (node :tag)
                         :-comment next-nodes
                         :-cdata   next-nodes
                         (element->rest-events node next-nodes))
    :else              next-nodes))

(defn flatten-elements
  "Flatten a collection of elements to an event seq"
  [elements]
  (when (seq elements)
    (lazy-seq
     (let [e (first elements)]
       (cons (node->first-event e)
             (flatten-elements (node->rest-events e (rest elements))))))))

;; "Emit" events to the in-memory representation

(defn parent-event->node
  [e children]
  (when (#{:start :empty} (-> e meta :clojure.data.xml/event))
    (assoc e :content (remove nil? children))))

(defn end-element-event?
  [e]
  (#{:end :empty} (-> e meta :clojure.data.xml/event)))

(defn event->node
  [e]
  (if (= :chars (-> e meta :clojure.data.xml/event))
    (first (e :content))
    e))

(defn event-tree
  "Returns a lazy tree of Element objects for the given seq of Event
  objects. See source-seq and parse."
  [events]
  (ffirst
   (seq-tree parent-event->node end-element-event? event->node events)))
