(ns gremid.data.xml.tree
  (:require
   [gremid.data.xml.event :as dx.event]
   [gremid.data.xml.node :as dx.node]
   [gremid.data.xml.name :as dx.name]))

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

(defn events->tree'
  [events parent-ns-ctx]
  (lazy-seq
   (when-let [[event] (seq events)]
     (let [more (rest events)]
       (if (dx.event/end? event)
         (cons nil more)
         (let [start? (dx.event/start? event)
               node   (dx.node/event->node event)
               node   (chars-node->str node)
               obj?   (map? node)
               ns-ctx    (dx.name/child-ns-ctx parent-ns-ctx event)
               tree   (events->tree' more ns-ctx)
               node   (cond-> node
                        obj?   (with-meta (dx.event/->metadata event ns-ctx))
                        start? (assoc :content (lazy-seq (first tree))))
               tree'  (if start?
                        (events->tree' (lazy-seq (rest tree)) ns-ctx)
                        tree)]
           (cons (cons node (lazy-seq (first tree')))
                 (lazy-seq (rest tree')))))))))

(defn events->tree
  [events]
  (ffirst (events->tree' events dx.name/initial-ns-ctx)))

(defn tree->events
  ([node]
   (tree->events node dx.name/initial-ns-ctx))
  ([node parent-ns-ctx]
   (let [node             (str->chars-node node)
         [start end ns-ctx] (dx.event/->objs node parent-ns-ctx)]
     (when start
       (lazy-cat
        [start]
        (mapcat #(tree->events % ns-ctx) (when end (:content node)))
        (some-> end list))))))
