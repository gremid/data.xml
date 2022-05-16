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
  [{[fchild] :content} ns-env]
  [(.createCharacters ef fchild) nil ns-env])

(defmethod ->objs :-cdata
  [{[fchild] :content} ns-env]
  [(.createCData ef fchild) nil ns-env])

(defmethod ->objs :-comment
  [{[fchild] :content} ns-env]
  [(.createComment ef fchild) nil ns-env])

(defmethod ->objs :-dtd
  [{[fchild] :content} ns-env]
  [(.createDTD ef fchild) nil ns-env])

(defmethod ->objs :-pi
  [{{:keys [target data]} :attrs} ns-env]
  [(.createProcessingInstruction ef target data) nil ns-env])

(defmethod ->objs :-document
  [{{:keys [encoding standalone]} :attrs} ns-env]
  [(if (nil? standalone)
     (if (nil? encoding)
       (.createStartDocument ef)
       (.createStartDocument ef encoding))
     (.createStartDocument ef (or encoding "UTF-8") "1.0" standalone))
   (.createEndDocument ef)
   ns-env])

(defmethod ->objs :-default
  [{:keys [tag attrs] :as node} ns-env]
  (when tag
    (let [attrs       (dx.name/separate-xmlns attrs)
          xmlns-attrs (first attrs)
          attrs       (second attrs)
          ns-env'     (dx.nss/compute ns-env node xmlns-attrs attrs)
          uri         (dx.name/qname-uri tag)
          local       (dx.name/qname-local tag)
          prefix      (dx.nss/get-prefix ns-env' uri)
          el-name     (if prefix (QName. uri local prefix) (QName. local))
          attributes  (for [[k v] attrs]
                        (let [uri   (dx.name/qname-uri k)
                              local (dx.name/qname-local k)]
                          (if (str/blank? uri)
                            (.createAttribute ef local v)
                            (.createAttribute ef (dx.nss/get-prefix ns-env' uri)
                                              uri local v))))
          namespaces  (into []
                            (map
                             (fn [[prefix uri]]
                               (if (str/blank? prefix)
                                 (.createNamespace ef uri)
                                 (.createNamespace ef prefix uri))))
                            (dx.nss/diff ns-env ns-env'))]
      [(.createStartElement ef el-name (.iterator ^Iterable attributes)
                            (.iterator ^Iterable namespaces))
       (.createEndElement ef el-name (.iterator ^Iterable namespaces))
       ns-env'])))
