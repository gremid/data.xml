;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.emit
  "JVM implementation of the emitter details"
  {:author "Herwig Hochleitner"}
  (:require
   [clojure.data.xml.name
    :refer [*gen-prefix-counter* gen-prefix qname-local qname-uri]]
   [clojure.data.xml.pu-map :as pu]
   [clojure.string :as str])
  (:import
   (java.io OutputStreamWriter StringWriter Writer)
   (java.nio.charset Charset)
   (java.util.logging Level Logger)
   (javax.xml.stream XMLOutputFactory XMLStreamWriter)))

(defn- emit-attrs
  [^XMLStreamWriter writer pu attrs]
  (reduce-kv
   (fn [_ attr value]
     (let [uri (qname-uri attr)
           local (qname-local attr)]
       (if (str/blank? uri)
         (.writeAttribute writer local (str value))
         (.writeAttribute writer (pu/get-prefix pu uri) uri local (str value))))
     _)
   nil attrs))

(defn- emit-ns-attrs
  [^XMLStreamWriter writer parent-pu pu]
  (pu/reduce-diff
   (fn [_ pf uri]
     (if (str/blank? pf)
       (.writeDefaultNamespace writer uri)
       (.writeNamespace writer pf uri))
     _)
   nil parent-pu pu))

(defn- compute-prefix
  [tpu uri suggested]
  (or (pu/get-prefix tpu uri)
      (loop [prefix (or suggested (gen-prefix))]
        (if (pu/get tpu prefix)
          (recur (gen-prefix))
          prefix))))

(def ^Logger logger
  (Logger/getLogger "clojure.data.xml"))

(defn- compute-pu
  [pu elem-pu attr-uris tag-uri tag-local]
  (let [tpu (pu/transient pu)
        ;; add namespaces from current environment
        tpu (reduce-kv (fn [tpu ns-attr uri]
                         (assert (string? ns-attr) (pr-str ns-attr uri))
                         (pu/assoc! tpu
                                    (compute-prefix tpu uri ns-attr)
                                    uri))
                       tpu (pu/prefix-map elem-pu))
        ;; add implicit namespaces used by tag, attrs
        tpu (reduce (fn [tpu uri]
                      (pu/assoc! tpu (compute-prefix tpu uri nil) uri))
                    tpu (if (str/blank? tag-uri)
                          attr-uris
                          (cons tag-uri attr-uris)))
        ;; rename default namespace, if tag is global (not in a namespace)
        tpu (if-let [uri (and (str/blank? tag-uri)
                              (pu/get tpu ""))]
              (do
                (when (.isLoggable logger Level/FINE)
                  (.log logger Level/FINE
                        (format (str "Default `xmlns=\"%s\"` had to be replaced "
                                     "with a `xmlns=\"\"` because of global "
                                     "element `%s`")
                                uri tag-local)))
                (-> tpu
                    (pu/assoc! "" "")
                    (as-> tpu (pu/assoc! tpu (compute-prefix tpu uri nil) uri))))
              tpu)]
    (pu/persistent! tpu)))

(defn- emit-start-tag
  [{:keys [attrs tag] :as e} ^XMLStreamWriter w pu-stack empty-element?]
  (let [uri       (qname-uri tag)
        local     (qname-local tag)
        parent-pu (first pu-stack)
        nss       (get (meta e) :clojure.data.xml/nss)
        pu        (compute-pu parent-pu nss (map qname-uri (keys attrs)) uri local)]
    (if empty-element?
      (do (if (str/blank? uri)
            (.writeEmptyElement w local)
            (.writeEmptyElement w (pu/get-prefix pu uri) local uri))
          (emit-ns-attrs w parent-pu pu)
          (emit-attrs w pu attrs)
          pu-stack)
      (do (if (str/blank? uri)
            (.writeStartElement w local)
            (.writeStartElement w (pu/get-prefix pu uri) local uri))
          (emit-ns-attrs w parent-pu pu)
          (emit-attrs w pu attrs)
          (cons pu pu-stack)))))

(defn emit-event
  [{:keys [content] :as e} ^XMLStreamWriter w pu-stack]
  (let [event (-> e meta :clojure.data.xml/event)]
    (condp = event
      :start   (emit-start-tag e w pu-stack false)
      :empty   (emit-start-tag e w pu-stack true)
      :end     (do (.writeEndElement w) (next pu-stack))
      (do
        (condp = event
          :chars   (some->> content first str (.writeCharacters w))
          :comment (some->> content first str (.writeComment w))
          :cdata   (some->> content first str (.writeCData w)))
        pu-stack))))

(defn check-stream-encoding
  [^OutputStreamWriter w xml-encoding]
  (when (not= (Charset/forName xml-encoding)
              (Charset/forName (.getEncoding w)))
    (throw
     (ex-info (str "Output encoding of writer ("
                   (.getEncoding w)
                   ") doesn't match declaration ("
                   xml-encoding ")")
              {:stream-encoding   (.getEncoding w)
               :declared-encoding xml-encoding}))))

(defn write-document
  "Writes the given event seq as XML text to writer.
   Options:
    :encoding <str>          Character encoding to use
    :doctype  <str>          Document type (DOCTYPE) declaration to use"
  [^Writer w events opts]
  (when (instance? OutputStreamWriter w)
    (check-stream-encoding w (or (:encoding opts) "UTF-8")))
  (binding [*gen-prefix-counter* 0]
    (let [^XMLStreamWriter sw (-> (XMLOutputFactory/newInstance)
                                  (.createXMLStreamWriter w))]
      (.writeStartDocument sw (or (:encoding opts) "UTF-8") "1.0")
      (when-let [doctype (:doctype opts)] (.writeDTD sw doctype))
      (reduce #(emit-event %2 sw %1) [pu/EMPTY] events)
      (.writeEndDocument sw)
      w)))

(defn string-writer
  []
  (StringWriter.))
