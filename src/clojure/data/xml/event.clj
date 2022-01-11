(ns clojure.data.xml.event
  "Data type for xml pull events"
  (:require
   [clojure.data.xml.impl :refer [compile-if extend-protocol-fns]]
   [clojure.data.xml.name :refer [separate-xmlns]]
   [clojure.data.xml.node :refer [cdata element* xml-comment]]
   [clojure.data.xml.protocols
    :refer [EventGeneration gen-event next-events xml-str]]
   [clojure.data.xml.pu-map :as pu])
  (:import
   (clojure.data.xml.node CData Comment Element)
   (clojure.lang IPersistentMap Keyword Sequential)
   (java.net URI URL)
   (java.util Date)
   (javax.xml.namespace QName)))

(definline element-nss* [element]
  `(get (meta ~element) :clojure.data.xml/nss pu/EMPTY))

(defn element-nss
  "Get xmlns environment from element"
  [{:keys [attrs] :as element}]
  (separate-xmlns
   attrs #(pu/merge-prefix-map (element-nss* element) %2)))

; Represents a parse event.
(defrecord StartElementEvent [tag attrs nss location-info])
(defrecord EmptyElementEvent [tag attrs nss location-info])
(defrecord CharsEvent [str])
(defrecord CDataEvent [str])
(defrecord CommentEvent [str])
(defrecord QNameEvent [qn])

;; EndElementEvent doesn't have any data, so make it a singleton
(deftype EndElementEvent [])
(def end-element-event (EndElementEvent.))
(defn ->EndElementEvent [] end-element-event)

;; Event Generation for stuff to show up in generated xml

(let [second-arg #(do %2)
      elem-event-generation
      {:gen-event (fn elem-gen-event [{:keys [tag attrs content] :as element}]
                    (separate-xmlns
                     attrs #((if (seq content)
                               ->StartElementEvent ->EmptyElementEvent)
                             tag %1 (pu/merge-prefix-map (element-nss* element) %2) nil)))
       :next-events (fn elem-next-events [{:keys [content]} next-items]
                      (if (seq content)
                        (list* content end-element-event next-items)
                        next-items))}
      string-event-generation {:gen-event (comp ->CharsEvent #'xml-str)
                               :next-events second-arg}
      qname-event-generation {:gen-event ->QNameEvent
                              :next-events second-arg}]
  (extend-protocol-fns
   EventGeneration
   (StartElementEvent EmptyElementEvent EndElementEvent CharsEvent CDataEvent CommentEvent)
   {:gen-event identity
    :next-events second-arg}
   (String Boolean Number (Class/forName "[B") Date URI URL nil)
   string-event-generation
   (Keyword QName) qname-event-generation
   CData
   {:gen-event (comp ->CDataEvent :content)
    :next-events second-arg}
   Comment
   {:gen-event (comp ->CommentEvent :content)
    :next-events second-arg}
   (IPersistentMap Element) elem-event-generation)
  (compile-if
   (Class/forName "java.time.Instant")
   (extend java.time.Instant
     EventGeneration
     string-event-generation)
   nil))

(extend-protocol EventGeneration
  Sequential
  (gen-event   [coll]
    (gen-event (first coll)))
  (next-events [coll next-items]
    (if-let [r (seq (rest coll))]
      (cons (next-events (first coll) r) next-items)
      (next-events (first coll) next-items))))

;; Node Generation for events

(defn event-element [event contents]
  (when (or (instance? StartElementEvent event)
            (instance? EmptyElementEvent event))
    (element* (:tag event) (:attrs event) contents
              (if-let [loc (:location-info event)]
                {:clojure.data.xml/location-info loc
                 :clojure.data.xml/nss (:nss event)}
                {:clojure.data.xml/nss (:nss event)}))))

(defn event-node [event]
  (cond
    (instance? CharsEvent event) (:str event)
    (instance? CDataEvent event) (cdata (:str event))
    (instance? CommentEvent event) (xml-comment (:str event))
    :else (throw (ex-info "Illegal argument, not an event object" {:event event}))))

(defn event-exit? [event]
  (identical? end-element-event event))
