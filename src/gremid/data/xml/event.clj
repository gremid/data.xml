(ns gremid.data.xml.event
  (:require
   [clojure.string :as str]
   [gremid.data.xml.name :as dx.name])
  (:import (javax.xml.namespace QName)
           (javax.xml.stream.events XMLEvent)
           (org.codehaus.stax2.evt XMLEventFactory2)))

(defn start?
  [^XMLEvent event]
  (or (.isStartElement event)
      (.isStartDocument event)))

(defn end?
  [^XMLEvent event]
  (or (.isEndElement event)
      (.isEndDocument event)))

(defn ->metadata
  [^XMLEvent event ns-ctx]
  (let [location (.getLocation event)
        location {:character-offset (.getCharacterOffset location)
                  :column-number    (.getColumnNumber location)
                  :line-number      (.getLineNumber location)}]
    {:gremid.data.xml/event         event
     :gremid.data.xml/location-info location
     :gremid.data.xml/ns-ctx           ns-ctx}))

(def ef
  (XMLEventFactory2/newInstance))

(defmulti ->objs
  (fn [node _] (:tag node))
  :default :-default)

(defmethod ->objs :-chars
  [{[fchild] :content} parent-ns-ctx]
  [(.createCharacters ef fchild) nil parent-ns-ctx])

(defmethod ->objs :-cdata
  [{[fchild] :content} parent-ns-ctx]
  [(.createCData ef fchild) nil parent-ns-ctx])

(defmethod ->objs :-comment
  [{[fchild] :content} parent-ns-ctx]
  [(.createComment ef fchild) nil parent-ns-ctx])

(defmethod ->objs :-dtd
  [{[fchild] :content} parent-ns-ctx]
  [(.createDTD ef fchild) nil parent-ns-ctx])

(defmethod ->objs :-pi
  [{{:keys [target data]} :attrs} parent-ns-ctx]
  [(.createProcessingInstruction ef target data) nil parent-ns-ctx])

(defmethod ->objs :-document
  [{{:keys [encoding standalone]} :attrs} parent-ns-ctx]
  [(if (nil? standalone)
     (if (nil? encoding)
       (.createStartDocument ef)
       (.createStartDocument ef encoding))
     (.createStartDocument ef (or encoding "UTF-8") "1.0" standalone))
   (.createEndDocument ef)
   parent-ns-ctx])

(defmethod ->objs :-default
  [node parent-ns-ctx]
  (when-let [tag (:tag node)]
    (let [attrs       (dx.name/separate-xmlns (:attrs node))
          xmlns-attrs (first attrs)
          attrs       (second attrs)
          ns-ctx         (or (:gremid.data.xml/ns-ctx node)
                          (:gremid.data.xml/ns-ctx (meta node))
                          parent-ns-ctx)
          ns-ctx         (dx.name/compute ns-ctx node xmlns-attrs attrs)
          uri         (dx.name/qname-uri tag)
          local       (dx.name/qname-local tag)
          prefix      (dx.name/get-prefix ns-ctx uri)
          el-name     (if prefix (QName. uri local prefix) (QName. local))
          attributes  (for [[k v] attrs]
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
