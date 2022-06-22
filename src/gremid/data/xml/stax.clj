(ns gremid.data.xml.stax
  (:require
   [clojure.string :as str]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.xform :as dx.xf])
  (:import (javax.xml.namespace QName)
           (javax.xml.stream.events Attribute Characters Comment DTD EntityDeclaration EntityReference NotationDeclaration ProcessingInstruction StartDocument StartElement XMLEvent)
           (org.codehaus.stax2.evt XMLEventFactory2)))

(defprotocol AsData
  (stax->data [event]))

(extend-protocol AsData
  StartDocument
  (stax->data
    [event]
    {:tag   :-document
     :attrs {:encoding   (when (.encodingSet event)
                           (.getCharacterEncodingScheme event))
             :standalone (when (.standaloneSet event)
                           (.isStandalone event))
             :system-id  (.getSystemId event)}})

  StartElement
  (stax->data
    [event]
    {:tag   (dx.name/as-qname (.getName event))
     :attrs (persistent!
             (reduce
              (fn [m ^Attribute attr]
                (assoc! m (dx.name/as-qname (.getName attr)) (.getValue attr)))
              (transient (array-map))
              (iterator-seq (.getAttributes event))))})

  Characters
  (stax->data
    [event]
    {:tag     (if (.isCData event) :-cdata :-chars)
     :attrs   {}
     :content (list (.getData event))})
  Comment
  (stax->data
    [event]
    {:tag     :-comment
     :attrs   {}
     :content (list (.getText event))})

  ProcessingInstruction
  (stax->data
    [event]
    {:tag     :-pi
     :attrs   {:target (.getTarget event)
               :data   (.getData event)}
     :content (list)})

  DTD
  (stax->data
    [event]
    {:tag     :-dtd
     :attrs   {}
     :content (list (.getDocumentTypeDeclaration event))})

  EntityDeclaration
  (stax->data
    [_]
    (throw (UnsupportedOperationException.)))

  EntityReference
  (stax->data
    [_]
    (throw (UnsupportedOperationException.)))

  NotationDeclaration
  (stax->data
    [_]
    (throw (UnsupportedOperationException.))))

(defn start?
  [^XMLEvent event]
  (or (.isStartElement event)
      (.isStartDocument event)))

(defn end?
  [^XMLEvent event]
  (or (.isEndElement event)
      (.isEndDocument event)))

(defn stax->data'
  [parent event]
  (let [parent-ns-ctx (or (some-> parent :gremid.data.xml/ns-ctx)
                          dx.name/initial-ns-ctx)
        ns-ctx        (dx.name/child-ns-ctx parent-ns-ctx event)
        location      (.getLocation event)
        location      {:character-offset (.getCharacterOffset location)
                       :column-number    (.getColumnNumber location)
                       :line-number      (.getLineNumber location)}]
    (assoc (stax->data event)
           :gremid.data.xml/event         event
           :gremid.data.xml/location-info location
           :gremid.data.xml/ns-ctx        ns-ctx)))

(def ->data-xf
  (comp
   (dx.xf/nesting
    start?
    end?
    (fn [[parent] event]
      (stax->data' parent event))
    (fn [[parent] event]
      (cond
        (start? event) (assoc parent :gremid.data.xml/start? true)
        (end? event)   (assoc parent :gremid.data.xml/end?   true)
        :else          (stax->data' parent event))))
   (dx.xf/nesting
    (fn [_ v] v)
    (fn [ctx v]
      (let [ctx   (cond
                    (:gremid.data.xml/start? v) (rest ctx)
                    (:gremid.data.xml/end?   v) (rest ctx)
                    :else                          ctx)
            depth (count ctx)]
        (assoc v
               :gremid.data.xml/ctx ctx
               :gremid.data.xml/depth depth))))))

(defn ->data
  [events]
  (sequence ->data-xf events))

(def ef
  (XMLEventFactory2/newInstance))

(defmulti data->stax
  (fn [v _] (:tag v))
  :default :-default)

(defmethod data->stax :-chars
  [{[fchild] :content} parent-ns-ctx]
  [(.createCharacters ef fchild) nil parent-ns-ctx])

(defmethod data->stax :-cdata
  [{[fchild] :content} parent-ns-ctx]
  [(.createCData ef fchild) nil parent-ns-ctx])

(defmethod data->stax :-comment
  [{[fchild] :content} parent-ns-ctx]
  [(.createComment ef fchild) nil parent-ns-ctx])

(defmethod data->stax :-dtd
  [{[fchild] :content} parent-ns-ctx]
  [(.createDTD ef fchild) nil parent-ns-ctx])

(defmethod data->stax :-pi
  [{{:keys [target data]} :attrs} parent-ns-ctx]
  [(.createProcessingInstruction ef target data) nil parent-ns-ctx])

(def end-document-event
  (.createEndDocument ef))

(defmethod data->stax :-document
  [{{:keys [encoding standalone]} :attrs} parent-ns-ctx]
  [(if (nil? standalone)
     (if (nil? encoding)
       (.createStartDocument ef)
       (.createStartDocument ef encoding))
     (.createStartDocument ef (or encoding "UTF-8") "1.0" standalone))
   end-document-event
   parent-ns-ctx])

(defmethod data->stax :-default
  [v parent-ns-ctx]
  (when-let [tag (:tag v)]
    (let [attrs       (dx.name/separate-xmlns (:attrs v))
          xmlns-attrs (first attrs)
          attrs       (second attrs)
          ns-ctx         (or (:gremid.data.xml/ns-ctx v)
                          (:gremid.data.xml/ns-ctx (meta v))
                          parent-ns-ctx)
          ns-ctx         (dx.name/compute ns-ctx v xmlns-attrs attrs)
          uri         (dx.name/qname-uri tag)
          local       (dx.name/qname-local tag)
          prefix      (dx.name/get-prefix ns-ctx uri)
          el-name     (if prefix (QName. uri local prefix) (QName. local))
          attributes  (for [[k v] attrs :when (some? v)]
                        (let [uri   (dx.name/qname-uri k)
                              local (dx.name/qname-local k)]
                          (if (str/blank? uri)
                            (.createAttribute ef local v)
                            (.createAttribute ef (dx.name/get-prefix ns-ctx uri)
                                              uri local v))))
          namespaces  (into []
                            (map
                             (fn [[prefix uri]]
                               (if (str/blank? prefix)
                                 (.createNamespace ef uri)
                                 (.createNamespace ef prefix uri))))
                            (dx.name/diff parent-ns-ctx ns-ctx))]
      [(.createStartElement ef el-name (.iterator ^Iterable attributes)
                            (.iterator ^Iterable namespaces))
       (.createEndElement ef el-name (.iterator ^Iterable namespaces))
       ns-ctx])))

(def ->stax-event-xf
  (dx.xf/nesting
   (fn [[[_ _ parent-ns-ctx]] v]
     (data->stax v (or parent-ns-ctx dx.name/initial-ns-ctx)))
   (fn [[[start-event end-event ns-ctx]] v]
     (cond
       (:gremid.data.xml/start? v) start-event
       (:gremid.data.xml/end?   v) end-event
       :else                          (first (data->stax v ns-ctx))))))

(defn ->stax-events
  [vs]
  (sequence ->stax-event-xf vs))
