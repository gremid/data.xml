(ns clojure.data.xml
  "Functions to parse XML into lazy sequences and lazy trees and emit these as
  text."
  (:require
   [clojure.data.xml.emit :refer [string-writer write-document]]
   [clojure.data.xml.name :refer [clj-ns-name separate-xmlns uri-symbol]]
   [clojure.data.xml.parse :refer [make-stream-reader pull-seq string-source]]
   [clojure.data.xml.pprint :refer [indent-xml]]
   [clojure.data.xml.prxml :refer [as-elements]]
   [clojure.data.xml.pu-map :as pu]
   [clojure.data.xml.tree :refer [event-tree flatten-elements]]))

(defn alias-uri
  "Define a Clojure namespace aliases for xmlns uris.

  This sets up the current namespace for reading qnames denoted with
  Clojure's ::alias/keywords reader feature.

  ## Example
  (alias-uri :D \"DAV:\")
                           ; similar in effect to
  ;; (require '[xmlns.DAV%3A :as D])
                           ; but required namespace is auto-created
                           ; henceforth, shorthand keywords can be used
  {:tag ::D/propfind}
                           ; ::D/propfind will be expanded to :xmlns.DAV%3A/propfind
                           ; in the current namespace by the reader"
  {:arglists '([& {:as alias-nss}])}
  [& ans]
  (loop [[a n & rst :as ans] ans]
    (when (seq ans)
      (let [xn (uri-symbol n)
            al (symbol (clj-ns-name a))]
        (create-ns xn)
        (alias al xn)
        (recur rst)))))

(defn sexps-as-fragment
  "Convert a compact prxml/hiccup-style data structure into the more formal
   tag/attrs/content format. A seq of elements will be returned, which may
   not be suitable for immediate use as there is no root element. See also
   sexp-as-element.

   The format is [:tag-name attr-map? content*]. Each vector opens a new tag;
   seqs do not open new tags, and are just used for inserting groups of elements
   into the parent tag. A bare keyword not in a vector creates an empty element.

   To provide XML conversion for your own data types, extend the AsElements
   protocol to them."
  ([] nil)
  ([sexp] (as-elements sexp))
  ([sexp & sexps] (mapcat as-elements (cons sexp sexps))))

(defn sexp-as-element
  "Convert a single sexp into an Element"
  [sexp]
  (let [[root & more] (sexps-as-fragment sexp)]
    (when more
      (throw
       (IllegalArgumentException.
        "Cannot have multiple root elements; try creating a fragment instead")))
    root))

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
