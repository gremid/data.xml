(ns gremid.data.xml.tree
  (:require
   [gremid.data.xml.event :as dx.event]
   [gremid.data.xml.node :as dx.node]
   [gremid.data.xml.nss :as dx.nss])
  (:import
   (javax.xml.stream.events Namespace XMLEvent)))

(defn chars-node->str
  [{:keys [tag] :as node}]
  (if (= :-chars tag)
    (-> node :content first)
    node))

(defn str->chars-node
  [node]
  (if (string? node)
    {:tag     :-chars
     :attrs   {}
     :content (list node)}
    node))

(defn child-nss
  [nss ^XMLEvent event]
  (reduce
   (fn [nss ^Namespace ns]
     (dx.nss/assoc' nss (.getPrefix ns) (.getNamespaceURI ns)))
   nss
   (when (.isStartElement event) (iterator-seq (.getNamespaces event)))))

(defn events->tree'
  [events nss]
  (lazy-seq
   (when-let [[event] (seq events)]
     (let [more (rest events)]
       (if (dx.event/end? event)
         (cons nil more)
         (let [start? (dx.event/start? event)
               node   (dx.node/event->node event)
               node   (chars-node->str node)
               obj?   (map? node)
               nss'   (child-nss nss event)
               tree   (events->tree' more nss')
               node   (cond-> node
                        obj?   (with-meta (dx.event/->metadata event nss'))
                        start? (assoc :content (lazy-seq (first tree))))
               tree'  (if start?
                        (events->tree' (lazy-seq (rest tree)) nss')
                        tree)]
           (cons (cons node (lazy-seq (first tree')))
                 (lazy-seq (rest tree')))))))))

(defn events->tree
  [events]
  (ffirst (events->tree' events dx.nss/EMPTY)))

(defn tree->events
  ([node]
   (tree->events node dx.nss/EMPTY))
  ([node ns-env]
   (let [node        (str->chars-node node)
         [start end ns-env'] (dx.event/->objs node ns-env)]
     (when start
       (lazy-cat
        [start]
        (mapcat #(tree->events % ns-env') (when end (:content node)))
        (some-> end list))))))
