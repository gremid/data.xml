(ns gremid.data.xml.event
  "JVM implementation of the emitter details"
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.pu-map :as dx.pu])
  (:import
   (javax.xml.namespace QName)
   (org.codehaus.stax2.evt XMLEventFactory2)))

(defn- compute-prefix
  [tpu uri suggested]
  (or (dx.pu/get-prefix tpu uri)
      (loop [prefix (or suggested (dx.name/gen-prefix))]
        (if (dx.pu/get tpu prefix)
          (recur (dx.name/gen-prefix))
          prefix))))

(defn- compute-pu
  [pu elem-pu attr-uris tag-uri tag-local]
  (let [tpu (dx.pu/transient pu)
        ;; add namespaces from current environment
        tpu (reduce-kv (fn [tpu ns-attr uri]
                         (assert (string? ns-attr) (pr-str ns-attr uri))
                         (dx.pu/assoc! tpu
                                    (compute-prefix tpu uri ns-attr)
                                    uri))
                       tpu (dx.pu/prefix-map elem-pu))
        ;; add implicit namespaces used by tag, attrs
        tpu (reduce (fn [tpu uri]
                      (dx.pu/assoc! tpu (compute-prefix tpu uri nil) uri))
                    tpu (if (str/blank? tag-uri)
                          attr-uris
                          (cons tag-uri attr-uris)))
        ;; rename default namespace, if tag is global (not in a namespace)
        tpu (if-let [uri (and (str/blank? tag-uri)
                              (dx.pu/get tpu ""))]
              (do
                (log/debugf (str "Default `xmlns=\"%s\"` had to be replaced "
                                 "with a `xmlns=\"\"` because of global "
                                 "element `%s`")
                            uri tag-local)
                (-> tpu
                    (dx.pu/assoc! "" "")
                    (as-> tpu (dx.pu/assoc! tpu (compute-prefix tpu uri nil) uri))))
              tpu)]
    (dx.pu/persistent! tpu)))

(def event-factory
  (XMLEventFactory2/newInstance))

(defn ->events
  ([node]
   (->events dx.pu/EMPTY node))
  ([parent-ns-env node]
   (if (string? node)
     (list (.createCharacters event-factory node))
     (when (map? node)
       (let [{:keys [tag attrs] [fchild :as content] :content} node]
         (condp = tag
           :-comment  (list (.createComment event-factory fchild))
           :-cdata    (list (.createCData event-factory fchild))
           :-pi       (list (.createProcessingInstruction event-factory
                                                         (attrs :target)
                                                         (attrs :data)))
           :-dtd      (list (.createDTD event-factory fchild))
           :-document (let [{:keys [encoding standalone]} attrs]
                        (lazy-cat
                         (list
                          (if (nil? standalone)
                            (if (nil? encoding)
                              (.createStartDocument event-factory)
                              (.createStartDocument event-factory encoding))
                            (.createStartDocument event-factory
                                                  (or encoding "UTF-8")
                                                  "1.0"
                                                  standalone)))
                         (mapcat ->events content)
                         (list (.createEndDocument event-factory))))
           (let [uri         (dx.name/qname-uri tag)
                 local       (dx.name/qname-local tag)
                 attrs'      (dx.name/separate-xmlns attrs)
                 attrs       (first attrs')
                 xmlns-attrs (second attrs')
                 el-ns-env   (get (meta node) :gremid.data.xml/nss dx.pu/EMPTY)
                 el-ns-env   (dx.pu/merge-prefix-map el-ns-env xmlns-attrs)
                 attr-uris   (map dx.name/qname-uri (keys attrs))
                 ns-env      (compute-pu parent-ns-env el-ns-env attr-uris uri local)
                 prefix      (dx.pu/get-prefix ns-env uri)
                 attributes  (for [[k v] attrs]
                               (let [uri   (dx.name/qname-uri k)
                                     local (dx.name/qname-local k)]
                                (if (str/blank? uri)
                                  (.createAttribute event-factory
                                                    local v)
                                  (.createAttribute event-factory
                                                    (dx.pu/get-prefix ns-env uri)
                                                    uri local v))))
                 namespaces  (dx.pu/reduce-diff
                             (fn [nss prefix uri]
                               (conj
                                nss
                                (if (str/blank? prefix)
                                  (.createNamespace event-factory uri)
                                  (.createNamespace event-factory prefix uri))))
                             [] parent-ns-env ns-env)]
             (lazy-cat
              (list
               (if prefix
                 (.createStartElement event-factory
                                      prefix uri local
                                      (.iterator ^Iterable attributes)
                                      (.iterator ^Iterable namespaces))
                 (.createStartElement event-factory
                                      (QName. local)
                                      (.iterator ^Iterable attributes)
                                      (.iterator ^Iterable namespaces))))
              (mapcat #(->events ns-env %) content)
              (list
               (if prefix
                 (.createEndElement event-factory
                                    prefix uri local
                                    (.iterator ^Iterable namespaces))
                 (.createEndElement event-factory
                                    (QName. local)
                                    (.iterator ^Iterable namespaces))))))))))))
