(ns gremid.data.xml.seq
  (:require [gremid.data.xml.event :as dx.event]
            [gremid.data.xml.node :as dx.node]
            [gremid.data.xml.nss :as dx.nss]))

(defn ctx-xf*
  "Create stateful transducers based on a stack representing the (nesting) XML
  element context for each transduced XML event.

  `sf` maps an element-start event to a value to be pushed on the stack. `f`
  yields the transduced value based on the current stack and an input event."
  ([start? end? sf f]
   (partial ctx-xf* start? end? sf f))
  ([start? end? sf f rf]
   (let [stack (atom (list))]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result evt]
        (cond
          (start? evt)
          (rf result (f (swap! stack conj (sf @stack evt)) evt))
          (end? evt)
          (let [r (rf result (f @stack evt))] (swap! stack pop) r)
          :else
          (rf result (f @stack evt))))))))

(defn event->data
  [parent event]
  (let [nss (or (some-> parent :gremid.data.xml/nss) dx.nss/EMPTY)]
    (merge (dx.node/event->node event)
           (dx.event/->metadata event nss))))

(def node-xf
  (ctx-xf*
   dx.event/start?
   dx.event/end?
   (fn [[parent] event]
     (event->data parent event))
   (fn [[parent] event]
     (cond
       (dx.event/start? event) (assoc parent :gremid.data.xml/start? true)
       (dx.event/end? event)   (assoc parent :gremid.data.xml/end?   true)
       :else                   (event->data parent event)))))

(def nesting-xf
  (ctx-xf*
   :gremid.data.xml/start?
   :gremid.data.xml/end?
   (fn [_ node] node)
   (fn [ctx node]
     (let [ctx   (cond
                   (:gremid.data.xml/start? node) (rest ctx)
                   (:gremid.data.xml/end?   node) (rest ctx)
                   :else                          ctx)
           depth (count ctx)]
       (assoc node
              :gremid.data.xml/ctx ctx
              :gremid.data.xml/depth depth)))))

(def seq-xf
  (comp
   node-xf
   nesting-xf))

(defn ->seq
  [events]
  (sequence seq-xf events))

(def event-obj-xf
  (ctx-xf*
   :gremid.data.xml/start?
   :gremid.data.xml/end?
   (fn [[_ _ parent-ns-env :as parent] node]
     (let [ns-env (or (:gremid.data.xml/nss node)
                      (when parent parent-ns-env)
                      dx.nss/EMPTY)]
       (dx.event/->objs node ns-env)))
   (fn [[[start-event end-event ns-env]] node]
     (cond
       (:gremid.data.xml/start? node) start-event
       (:gremid.data.xml/end?   node) end-event
       :else                          (first (dx.event/->objs node ns-env))))))

(defn seq->events
  [events]
  (sequence event-obj-xf events))
