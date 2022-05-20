(ns gremid.data.xml.event
  (:require
   [clojure.string :as str]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.nss :as dx.nss])
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
  [^XMLEvent event nss]
  (let [location (.getLocation event)
        location {:character-offset (.getCharacterOffset location)
                  :column-number    (.getColumnNumber location)
                  :line-number      (.getLineNumber location)}]
    {:gremid.data.xml/event         event
     :gremid.data.xml/location-info location
     :gremid.data.xml/nss           nss}))

(def ef
  (XMLEventFactory2/newInstance))

(defmulti ->objs
  (fn [node _] (:tag node))
  :default :-default)

(defmethod ->objs :-chars
  [{[fchild] :content} parent-nss]
  [(.createCharacters ef fchild) nil parent-nss])

(defmethod ->objs :-cdata
  [{[fchild] :content} parent-nss]
  [(.createCData ef fchild) nil parent-nss])

(defmethod ->objs :-comment
  [{[fchild] :content} parent-nss]
  [(.createComment ef fchild) nil parent-nss])

(defmethod ->objs :-dtd
  [{[fchild] :content} parent-nss]
  [(.createDTD ef fchild) nil parent-nss])

(defmethod ->objs :-pi
  [{{:keys [target data]} :attrs} parent-nss]
  [(.createProcessingInstruction ef target data) nil parent-nss])

(defmethod ->objs :-document
  [{{:keys [encoding standalone]} :attrs} parent-nss]
  [(if (nil? standalone)
     (if (nil? encoding)
       (.createStartDocument ef)
       (.createStartDocument ef encoding))
     (.createStartDocument ef (or encoding "UTF-8") "1.0" standalone))
   (.createEndDocument ef)
   parent-nss])

(defmethod ->objs :-default
  [node parent-nss]
  (when-let [tag (:tag node)]
    (let [attrs       (dx.name/separate-xmlns (:attrs node))
          xmlns-attrs (first attrs)
          attrs       (second attrs)
          nss         (or (:gremid.data.xml/nss node)
                          (:gremid.data.xml/nss (meta node))
                          parent-nss)
          nss         (dx.nss/compute nss node xmlns-attrs attrs)
          uri         (dx.name/qname-uri tag)
          local       (dx.name/qname-local tag)
          prefix      (dx.nss/get-prefix nss uri)
          el-name     (if prefix (QName. uri local prefix) (QName. local))
          attributes  (for [[k v] attrs]
                        (let [uri   (dx.name/qname-uri k)
                              local (dx.name/qname-local k)]
                          (if (str/blank? uri)
                            (.createAttribute ef local v)
                            (.createAttribute ef (dx.nss/get-prefix nss uri)
                                              uri local v))))
          namespaces  (into []
                            (map
                             (fn [[prefix uri]]
                               (if (str/blank? prefix)
                                 (.createNamespace ef uri)
                                 (.createNamespace ef prefix uri))))
                            (dx.nss/diff parent-nss nss))]
      [(.createStartElement ef el-name (.iterator ^Iterable attributes)
                            (.iterator ^Iterable namespaces))
       (.createEndElement ef el-name (.iterator ^Iterable namespaces))
       nss])))
