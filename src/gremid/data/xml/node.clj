(ns gremid.data.xml.node
  (:require
   [gremid.data.xml.name :as dx.name])
  (:import
   (javax.xml.stream.events Attribute Characters Comment DTD EntityDeclaration EntityReference NotationDeclaration ProcessingInstruction StartDocument StartElement)))

(defn document
  [& content]
  {:tag     :-document
   :attrs   {:encoding   nil
             :standalone nil
             :system-id  nil}
   :content content})

(defn element
  ([tag]
   {:tag     tag
    :attrs   {}
    :content (list)})
  ([tag attrs & content]
   {:tag     tag
    :attrs   (or attrs {})
    :content (or content (list))}))

(defn doc-element
  [doc]
  (some->> doc :content (filter :tag) last))

(defn dtd
  [s]
  (element :-dtd {} s))

(defn cdata
  [s]
  (element :-cdata {} s))

(defn pi
  [target data]
  (element :-pi {:target target :data data}))

(defn xml-comment
  [s]
  (element :-comment {} s))

(defprotocol AsNode
  (event->node [event]))

(extend-protocol AsNode
  StartDocument
  (event->node
    [event]
    {:tag   :-document
     :attrs {:encoding   (when (.encodingSet event)
                           (.getCharacterEncodingScheme event))
             :standalone (when (.standaloneSet event)
                           (.isStandalone event))
             :system-id  (.getSystemId event)}})

  StartElement
  (event->node
    [event]
    {:tag   (dx.name/as-qname (.getName event))
     :attrs (persistent!
             (reduce
              (fn [m ^Attribute attr]
                (assoc! m (dx.name/as-qname (.getName attr)) (.getValue attr)))
              (transient {})
              (iterator-seq (.getAttributes event))))})

  Characters
  (event->node
    [event]
    {:tag     (if (.isCData event) :-cdata :-chars)
     :attrs   {}
     :content (list (.getData event))})
  Comment
  (event->node
    [event]
    {:tag     :-comment
     :attrs   {}
     :content (list (.getText event))})

  ProcessingInstruction
  (event->node
    [event]
    {:tag     :-pi
     :attrs   {:target (.getTarget event)
               :data   (.getData event)}
     :content (list)})

  DTD
  (event->node
    [event]
    {:tag     :-dtd
     :attrs   {}
     :content (list (.getDocumentTypeDeclaration event))})

  EntityDeclaration
  (event->node
    [_]
    (throw (UnsupportedOperationException.)))

  EntityReference
  (event->node
    [_]
    (throw (UnsupportedOperationException.)))

  NotationDeclaration
  (event->node
    [_]
    (throw (UnsupportedOperationException.))))
