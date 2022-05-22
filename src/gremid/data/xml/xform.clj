(ns gremid.data.xml.xform)

(defn nesting
  "Create stateful transducers based on a stack representing the (nesting) XML
  element context for each transduced XML event.

  `sf` maps an element-start event to a value to be pushed on the stack. `f`
  yields the transduced value based on the current stack and an input event."
  ([sf f]
   (nesting :gremid.data.xml/start? :gremid.data.xml/end? sf f))
  ([start? end? sf f]
   (partial nesting start? end? sf f))
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
