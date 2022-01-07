;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml
  "Functions to parse XML into lazy sequences and lazy trees and emit these as
  text."
  {:author "Chris Houser"}
  (:require
   [clojure.data.xml.emit :refer [string-writer write-document]]
   [clojure.data.xml.impl :refer [export-api]]
   [clojure.data.xml.name :as name :refer [separate-xmlns]]
   [clojure.data.xml.node :as node]
   [clojure.data.xml.parse
    :refer [make-stream-reader pull-seq string-source]]
   [clojure.data.xml.pprint :refer [indent-xml]]
   [clojure.data.xml.process :as process]
   [clojure.data.xml.prxml :as prxml]
   [clojure.data.xml.pu-map :as pu]
   [clojure.data.xml.tree :refer [event-tree flatten-elements]]))

(export-api node/element* node/element node/cdata node/xml-comment node/element?
            prxml/sexp-as-element prxml/sexps-as-fragment
            name/alias-uri name/parse-qname name/qname-uri
            name/qname-local name/qname name/as-qname
            name/uri-symbol name/symbol-uri
            process/find-xmlns process/aggregate-xmlns)

(defn element-nss
  "Get xmlns environment from element"
  [{:keys [attrs] :as element}]
  (pu/merge-prefix-map
   (-> element meta :clojure.data.xml/nss)
   (second (separate-xmlns attrs))))

(def ^:private ^:const parser-opts-arg
  '{:keys [include-node? location-info
           coalescing supporting-external-entities
           allocator namespace-aware replacing-entity-references
           validating reporter resolver support-dtd]
    :or {include-node? #{:element :characters}
         location-info true
         coalescing true
         supporting-external-entities false}})

(defn event-seq
  "Parses an XML input source into a lazy sequence of pull events.

Input source can be a java.io.InputStream or java.io.Reader

Options:

  :include-node? subset of #{:element :characters :comment}, default #{:element :characters}
  :location-info pass false to skip generating location meta data, default true

See https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLInputFactory.html
for documentation on xml options. These are the defaults:

  {:allocator                    nil      ; XMLInputFactory/ALLOCATOR
   :coalescing                   true     ; XMLInputFactory/IS_COALESCING
   :namespace-aware              true     ; XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references  true     ; XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities false    ; XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating                   false    ; XMLInputFactory/IS_VALIDATING
   :reporter                     nil      ; XMLInputFactory/REPORTER
   :resolver                     nil      ; XMLInputFactory/RESOLVER
   :support-dtd                  true     ; XMLInputFactory/SUPPORT_DTD
   }"
  {:arglists (list ['source parser-opts-arg])}
  [source opts]
  (let [props* (merge {:include-node? #{:element :characters}
                       :coalescing true
                       :supporting-external-entities false
                       :location-info true}
                      opts)]
    (pull-seq (make-stream-reader props* source)
              props*
              nil)))

(defn parse
  "Parses an XML input source into a a tree of Element records.
The element tree is realized lazily, so huge XML files can be streamed through a depth-first tree walk.

Input source can be a java.io.InputStream or java.io.Reader

Options:

  :include-node? subset of #{:element :characters :comment}, default #{:element :characters}
  :location-info pass false to skip generating location meta data, default true

See https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLInputFactory.html
for documentation on xml options. These are the defaults:

  {:allocator                    nil      ; XMLInputFactory/ALLOCATOR
   :coalescing                   true     ; XMLInputFactory/IS_COALESCING
   :namespace-aware              true     ; XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references  true     ; XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities false    ; XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating                   false    ; XMLInputFactory/IS_VALIDATING
   :reporter                     nil      ; XMLInputFactory/REPORTER
   :resolver                     nil      ; XMLInputFactory/RESOLVER
   :support-dtd                  true     ; XMLInputFactory/SUPPORT_DTD
   }"
  {:arglists (list ['source '& parser-opts-arg])}
  [source & {:as opts}]
  (event-tree (event-seq source opts)))

(defn parse-str
  "Parses an XML String into a a tree of Element records.

Options:

  :include-node? subset of #{:element :characters :comment}, default #{:element :characters}
  :location-info pass false to skip generating location meta data, default true

See https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLInputFactory.html
for documentation on xml options. These are the defaults:

  {:allocator                    nil      ; XMLInputFactory/ALLOCATOR
   :coalescing                   true     ; XMLInputFactory/IS_COALESCING
   :namespace-aware              true     ; XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references  true     ; XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities false    ; XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating                   false    ; XMLInputFactory/IS_VALIDATING
   :reporter                     nil      ; XMLInputFactory/REPORTER
   :resolver                     nil      ; XMLInputFactory/RESOLVER
   :support-dtd                  true     ; XMLInputFactory/SUPPORT_DTD
   }"
  {:arglists (list ['string '& parser-opts-arg])}
  [s & opts]
  (apply parse (string-source s) opts))

(defn emit
  "Prints the given Element tree as XML text to stream.
   Options:
    :encoding <str>          Character encoding to use
    :doctype  <str>          Document type (DOCTYPE) declaration to use"
  [e writer & {:as opts}]
  (write-document writer (flatten-elements [e]) opts))

(defn emit-str
  "Emits the Element to String and returns it.
   Options:
    :encoding <str>          Character encoding to use
    :doctype  <str>          Document type (DOCTYPE) declaration to use"
  ([e & opts]
   (let [sw (string-writer)]
     (apply emit e sw opts)
     (str sw))))

(defn indent
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for
   debugging/testing only."
  [e writer & opts]
  (indent-xml (apply emit-str e opts) writer))

(defn indent-str
  "Emits the XML and indents the result.  Writes the results to a String and returns it"
  [e & opts]
  (let [sw (string-writer)]
    (apply indent e sw opts)
    (str sw)))

;; TODO implement ~normalize to simulate an emit-parse roundtrip
;;      in terms of xmlns environment and keywords vs qnames
