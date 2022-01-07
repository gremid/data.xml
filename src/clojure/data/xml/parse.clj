;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.parse
  (:require
   [clojure.data.xml.name :refer [qname]]
   [clojure.data.xml.pu-map :as pu])
  (:import
   (java.io InputStream Reader)
   (javax.xml.stream XMLInputFactory XMLStreamReader)))

(def ^{:private true} input-factory-props
  {:allocator                    XMLInputFactory/ALLOCATOR
   :coalescing                   XMLInputFactory/IS_COALESCING
   :namespace-aware              XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references  XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating                   XMLInputFactory/IS_VALIDATING
   :reporter                     XMLInputFactory/REPORTER
   :resolver                     XMLInputFactory/RESOLVER
   :support-dtd                  XMLInputFactory/SUPPORT_DTD})

(defn- attr-hash
  [^XMLStreamReader sreader]
  (persistent!
   (reduce (fn [tr i]
             (assoc! tr (qname (.getAttributeNamespace sreader i)
                               (.getAttributeLocalName sreader i)
                               (.getAttributePrefix sreader i))
                     (.getAttributeValue sreader i)))
           (transient {})
           (range (.getAttributeCount sreader)))))

(defn- nss-hash
  [^XMLStreamReader sreader parent-hash]
  (pu/persistent!
   (reduce (fn [tr ^long i]
             (pu/assoc! tr
                        (.getNamespacePrefix sreader i)
                        (.getNamespaceURI ^XMLStreamReader sreader i)))
           (pu/transient parent-hash)
           (range (.getNamespaceCount sreader)))))

(defn- location-hash
  [^XMLStreamReader sreader]
  (when-let [location (.getLocation sreader)]
    {:character-offset (.getCharacterOffset location)
     :column-number    (.getColumnNumber location)
     :line-number      (.getLineNumber location)}))

(defn pull-seq
  "Creates a seq of events."
  [^XMLStreamReader sreader {:keys [include-node? location-info skip-whitespace] :as opts} ns-envs]
  ;; Note, sreader is mutable and mutated here in pull-seq, but it's protected
  ;; by a lazy-seq so it's thread-safe.
  (lazy-seq
   (loop []
     (let [location (when location-info (location-hash sreader))]
       (condp = (and (.hasNext sreader) (.next sreader))
         XMLStreamReader/START_ELEMENT
         (if (include-node? :element)
           (let [ns-env (nss-hash sreader (or (first ns-envs) pu/EMPTY))]
             (cons
              (with-meta
                {:tag   (qname (.getNamespaceURI sreader)
                               (.getLocalName sreader)
                               (.getPrefix sreader))
                 :attrs (attr-hash sreader)}
                {:clojure.data.xml/event    :start
                 :clojure.data.xml/location location
                 :clojure.data.xml/nss      ns-env})
              (pull-seq sreader opts (cons ns-env ns-envs))))
           (recur))
         XMLStreamReader/END_ELEMENT
         (if (include-node? :element)
           (cons
            (with-meta
              {}
              {:clojure.data.xml/event    :end
               :clojure.data.xml/location location})
            (pull-seq sreader opts (rest ns-envs)))
           (recur))
         XMLStreamReader/CHARACTERS
         (if-let [text (and (include-node? :characters)
                            (not (and skip-whitespace (.isWhiteSpace sreader)))
                            (.getText sreader))]
           (if (zero? (.length ^CharSequence text))
             (recur)
             (cons
              (with-meta
                {:content [text]}
                {:clojure.data.xml/event    :chars
                 :clojure.data.xml/location location})
              (pull-seq sreader opts ns-envs)))
           (recur))
         XMLStreamReader/COMMENT
         (if (include-node? :comment)
           (cons
            (with-meta
              {:tag     :-comment
               :content [(.getText sreader)]}
              {:clojure.data.xml/event    :comment
               :clojure.data.xml/location location})
            (pull-seq sreader opts ns-envs))
           (recur))
         ;; end of stream
         false
         nil
         ;; Consume and ignore comments, spaces, processing instructions etc
         (recur))))))

(defn- make-input-factory ^XMLInputFactory
  [props]
  (let [fac (XMLInputFactory/newInstance)]
    (doseq [[k v] props
            :when (contains? input-factory-props k)
            :let  [prop (input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn make-stream-reader
  [props source]
  (let [fac (make-input-factory props)]
    (cond
      (instance? Reader source)      (.createXMLStreamReader fac ^Reader source)
      (instance? InputStream source) (.createXMLStreamReader fac ^InputStream source)
      :else                          (throw (IllegalArgumentException.
                                             "source should be java.io.Reader or java.io.InputStream")))))

(defn string-source [s]
  (java.io.StringReader. s))
