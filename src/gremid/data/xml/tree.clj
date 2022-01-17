(ns gremid.data.xml.tree
  (:require
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.pu-map :as dx.pu])
  (:import
   (javax.xml.stream.events Attribute Characters Comment DTD EndDocument EndElement Namespace ProcessingInstruction StartDocument StartElement XMLEvent)))

(defprotocol AsTree
  (as-parent [event content ns-env] "Must return `nil` or `false` if the event
  is not an enter-sub-tree event. Any other return value will become a sub-tree
  of the output tree and should normally contain in some way the `content`.")

  (exit? [event] "Returns `true` for any event that marks the end of a
  sub-tree.")

  (->node [event] "Called on every event that is neither parent nor exit, and
  its return value will become a node of the output tree."))


(defn seq-tree
  "Takes a seq of events that logically represents a tree by each event being one
  of: enter-sub-tree event, exit-sub-tree event, or node event.

  Returns a lazy sequence whose first element is a sequence of sub-trees and
  whose remaining elements are events that are not siblings or descendants of
  the initial event."
  ([coll]
   (seq-tree coll dx.pu/EMPTY))
  ([coll ns-env]
   (lazy-seq
    (when-let [[event] (seq coll)]
      (let [more (rest coll)]
        (if (exit? event)
          (cons nil more)
          (let [tree (seq-tree more ns-env)]
            (if-let [p (as-parent event (lazy-seq (first tree)) ns-env)]
              (let [ns-env (get (meta p) :gremid.data.xml/nss)
                    subtree (seq-tree (lazy-seq (rest tree)) ns-env)]
                (cons (cons p (lazy-seq (first subtree)))
                      (lazy-seq (rest subtree))))
              (cons (cons (->node event) (lazy-seq (first tree)))
                    (lazy-seq (rest tree)))))))))))

(defn ->tree
  "Returns a lazy tree of Element objects for the given seq of Event
  objects. See source-seq and parse."
  [events]
  (ffirst (seq-tree events)))

;; "Parse" events off the in-memory representation

(defn assoc-location
  [^XMLEvent event o]
  (let [location (.getLocation event)]
    (vary-meta o assoc :gremid.data.xml/location-info
               {:character-offset (.getCharacterOffset location)
                :column-number    (.getColumnNumber location)
                :line-number      (.getLineNumber location)})))

(extend-protocol AsTree
  StartDocument
  (as-parent [^StartDocument event content ns-env]
    (->>
     (with-meta
       {:tag     :-document
        :attrs   {:encoding   (when (.encodingSet event)
                              (.getCharacterEncodingScheme event))
                  :standalone (when (.standaloneSet event)
                                (.isStandalone event))
                  :system-id  (.getSystemId event)}
        :content content}
       {:gremid.data.xml/nss ns-env})
     (assoc-location event)))
  (exit? [event]
    false)
  (->node [event])

  StartElement
  (as-parent [^StartElement event content ns-env]
    (->>
     (with-meta
       {:tag     (dx.name/as-qname (.getName event))
        :attrs   (persistent!
                  (reduce
                   (fn [m ^Attribute attr]
                     (assoc! m (dx.name/as-qname (.getName attr))
                             (.getValue attr)))
                   (transient {})
                   (iterator-seq (.getAttributes event))))
        :content content}
       {:gremid.data.xml/nss (dx.pu/persistent!
                              (reduce
                               (fn [ns-env ^Namespace ns]
                                 (dx.pu/assoc! ns-env
                                            (.getPrefix ns)
                                            (.getNamespaceURI ns)))
                               (dx.pu/transient ns-env)
                               (iterator-seq (.getNamespaces event))))})
     (assoc-location event)))
  (exit? [_] false)
  (->node [_])

  EndDocument
  (as-parent [_ _ _])
  (exit? [_] true)
  (->node [_])

  EndElement
  (as-parent [_ _ _])
  (exit? [_] true)
  (->node [_])

  Characters
  (as-parent [_ _ _])
  (exit? [_] false)
  (->node [^Characters event]
    (let [text (.getData event)]
      (if (.isCData event)
        (->>
         {:tag     :-cdata
          :attrs   {}
          :content (list text)}
         (assoc-location event))
        text)))

  ProcessingInstruction
  (as-parent [_ _ _])
  (exit? [_] false)
  (->node [^ProcessingInstruction event]
    (->>
     {:tag     :-pi
      :attrs   {:target (.getTarget event)
                :data   (.getData event)}
      :content (list)}
     (assoc-location event)))

  Comment
  (as-parent [_ _ _])
  (exit? [_] false)
  (->node [^Comment event]
    (->>
     {:tag     :-comment
      :attrs   {}
      :content (list (.getText event))}
     (assoc-location event)))

  DTD
  (as-parent [_ _ _])
  (exit? [_] false)
  (->node [^DTD event]
    (->>
     {:tag     :-dtd
      :attrs   {}
      :content (list (.getDocumentTypeDeclaration event))}
     (assoc-location event)))

  Object
  (as-parent [_ _ _])
  (exit? [_] false)
  (->node [event] event))
