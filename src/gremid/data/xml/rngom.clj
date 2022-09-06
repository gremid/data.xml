(ns gremid.data.xml.rngom
  "RELAX NG schema parser, based on Kohsuke Kawaguchi's RNG-OM."
  (:require
   [gremid.data.xml.io :as xml.io])
  (:import
   (javax.xml.namespace QName)
   (org.kohsuke.rngom.ast.util CheckingSchemaBuilder)
   (org.kohsuke.rngom.digested DAttributePattern DElementPattern DEmptyPattern DPattern DPatternWalker DRefPattern DSchemaBuilderImpl DValuePattern DXmlTokenPattern)
   (org.kohsuke.rngom.nc NameClass NameClassWalker)
   (org.kohsuke.rngom.parse.xml SAXParseable)
   (org.xml.sax.helpers DefaultHandler)))

;; ## Pattern predicates

(def xml-token-pattern?
  (partial instance? DXmlTokenPattern))

(def element-pattern?
  (partial instance? DElementPattern))

(def attribute-pattern?
  (partial instance? DAttributePattern))

(def value-pattern?
  (partial instance? DValuePattern))

(def empty-pattern?
  (partial instance? DEmptyPattern))

;; ## Parsing and schema traversal

(defn parse-schema
  "Parses a RELAX NG schema in XML syntax."
  [src]
  (let [eh             (proxy [DefaultHandler] [] (error [e] (throw e)))
        schema-builder (CheckingSchemaBuilder. (DSchemaBuilderImpl.) eh)
        schema (xml.io/as-input-source src)
        schema (SAXParseable. schema eh)]
    (.. schema (parse schema-builder))))

(defn traverse
  "Traverse the schema graph, starting at a given pattern, following
  references and returning a vector of traversed patterns.

  With a given predicate `descend?`, traversal stops at XML tokens not
  fulfilling the predicate."
  ([^DPattern start]
   (traverse start (constantly true)))
  ([^DPattern start descend?]
   (let [patterns (transient [])
         add! (fn [p] (conj! patterns p) nil)
         seen-refs (transient #{})]
     (->>
      (proxy [DPatternWalker] []
        (onXmlToken [^DXmlTokenPattern p]
          (add! p)
          (when (descend? p)
            (.. p (getChild) (accept this))))
        (onData [p] (add! p))
        (onEmpty [p] (add! p))
        (onText [p] (add! p))
        (onValue [p] (add! p))
        (onNotAllowed [p] (add! p))
        (onRef
          [^DRefPattern rp]
          (let [name (.getName rp)]
            (when-not (seen-refs name)
              (conj! seen-refs name)
              (.. rp (getTarget) (getPattern) (accept this))))))
      (.accept start))
     (persistent! patterns))))

(defn traverse-children
  "Traverses the schema, starting at a given pattern and curtailing the graph by
  stopping traversal on descendant XML tokens.

  This way, only child entities (elements, attributes, data, text and value
  pattern) are returned."
  [^DPattern start]
  (traverse start (some-fn #{start} (complement xml-token-pattern?))))

(defn walk-names
  [^NameClass name-class]
  (let [patterns (transient [])
        add!     (fn [m] (conj! patterns m) nil)]
    (->>
     (proxy [NameClassWalker] []
       (visitName [^QName qn]
         (add! qn))
       (visitAnyName [])
       (visitNsName [ns])
       (visitAnyNameExcept [nc])
       (visitNsNameExcept [ns nc])
       (visitNull []))
     (.accept name-class))
    (into #{} (persistent! patterns))))

(defn parse-names
  "Parses name class of a given XML Token, returning the set of referenced
  qualified names.

  (Does not handle exception classes.)"
  [^DXmlTokenPattern token-pattern]
  (walk-names (.getName token-pattern)))

;; ## Extract value sets, e.g. for attributes

(defn has-name?
  [n xml-token]
  (->> (parse-names xml-token) (map str) (some #{n})))

(defn element?
  [name]
  (every-pred element-pattern? (partial has-name? name)))

(defn attribute?
  [name]
  (every-pred attribute-pattern? (partial has-name? name)))

(defn values
  [patterns]
  (for [pattern patterns :when (value-pattern? pattern)]
    (.getValue ^DValuePattern pattern)))

(defn attribute-values
  "Exracts values for a given element/attribute combination."
  [element-name attribute-name patterns]
  (let [element? (element? element-name)
        attribute? (attribute? attribute-name)]
    (for [element patterns :when (element? element)
          attribute (traverse-children element) :when (attribute? attribute)
          value (values (traverse attribute))]
      value)))

;; ## Identify container and (text-)content elements

(defn container-element?
  "Patterns, which only have XML tokens as child patterns, describe container
  elements, i. e. elements without text content (apart from whitespace)."
  [^DElementPattern pattern]
  (->> (traverse-children pattern)
       (remove xml-token-pattern?)
       (remove empty-pattern?)
       (empty?)))

(defn local-names
  [pattern]
  (map #(.getLocalPart ^QName %) (parse-names pattern)))

(defn classify-elements
  [grammar]
  (let [patterns (traverse grammar)
        elements (filter element-pattern? patterns)
        elements (group-by container-element? elements)]
    {:containers (into (sorted-set) (mapcat local-names) (elements true))
     :content    (into (sorted-set) (mapcat local-names) (elements false))}))

