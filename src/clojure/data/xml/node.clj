(ns clojure.data.xml.node
  "Data types for xml nodes: Element, CData and Comment"
  (:require
   [clojure.data.xml.name :refer [as-qname]])
  (:import
   (clojure.lang APersistentMap IHashEq IObj IPersistentMap MapEntry MapEquivalence)
   (java.io Serializable Writer)
   (java.util Iterator Map)))

;; Parsed data format
;; Represents a node of an XML tree

;; We implement a custom deftype for elements
;; it is similar to (defrecord Element [tag attrs content])
;; but we override its hash and equality to be compatible with
;; clojure's hash-maps
;; see https://clojure.atlassian.net/browse/CLJ-2084
;; also, elements don't have an extmap and degrade to hash-maps also
;; when assoc'ing unknown keys

;; FIXME hash caching cannot be used: https://clojure.atlassian.net/browse/CLJ-2092

(deftype ElementIterator [el ^:volatile-mutable fields]
  Iterator
  (hasNext [_] (boolean (seq fields)))
  (next [_]
    (let [f (first fields)]
      (set! fields (next fields))
      (MapEntry. f (get el f)))))

(deftype Element [tag attrs content meta]

  ;; serializing/cloning, hashing, equality, iteration
  Serializable
  MapEquivalence
  IHashEq
  (hasheq [this] (APersistentMap/mapHasheq this))
  Iterable
  (iterator [this] (ElementIterator. this '(:tag :attrs :content)))
  Object
  (toString [_]
    (let [qname (as-qname tag)]
      (apply str (concat ["<" qname]
                         (mapcat (fn [[n a]]
                                   [" " (as-qname n) "=" (pr-str a)])
                                 attrs)
                         (if (seq content)
                           (concat [">"] content ["</" qname ">"])
                           ["/>"])))))
  (hashCode [this] (APersistentMap/mapHash this))
  (equals [this other] (APersistentMap/mapEquals this other))
  IPersistentMap
  (equiv [this other] (APersistentMap/mapEquals this other))

  ;; Main collection interfaces, that are included in IPersistentMap,
  ;; but are separate protocols in cljs

  (valAt  [this k]
    (.valAt this k nil))

  (valAt [_ k nf]
    (case k
      :tag     tag
      :attrs   attrs
      :content content
      nf))
  (count [_] 3)
  (cons [_ entry] 
    (conj (with-meta {:tag tag :attrs attrs :content content} meta)
          entry))
  (assoc [_ k v]
    (case k
      :tag (Element. v attrs content meta)
      :attrs (Element. tag v content meta)
      :content (Element. tag attrs v meta)
      (with-meta {:tag tag :attrs attrs :content content k v} meta)))
  (without [this k]
    (with-meta
      (case k
        :tag {:attrs attrs :content content}
        :attrs {:tag tag :content content}
        :content {:tag tag :attrs attrs}
        this)
      meta))
  (seq [this] (iterator-seq (.iterator this)))
  (empty [_] (Element. tag {} [] {}))

  ;; j.u.Map and included interfaces
  Map
  (entrySet [this] (set this))
  (values [this] (vals this))
  (keySet [this] (set (keys this)))
  (get [this k] (.valAt this k))
  (containsKey [_ k] (case k (:tag :attrs :content) true false))
  (containsValue [this v] (boolean (some #{v} (vals this))))
  (isEmpty [_] false)
  (size [_] 3)

  ;; Metadata interface
  IObj
  (meta [_] meta)
  (withMeta [_ next-meta]
    (Element. tag attrs content next-meta)))

(defmethod print-method Element [{:keys [tag attrs content]} ^Writer writer]
  (.write writer "#xml/element{:tag ")
  (print-method tag writer)
  (when-not (empty? attrs)
    (.write writer ", :attrs ")
    (print-method attrs writer))
  (when-not (empty? content)
    (.write writer ", :content [")
    (print-method (first content) writer)
    (doseq [c (next content)]
      (.write writer " ")
      (print-method c writer))
    (.write writer "]"))
  (.write writer "}"))

(defrecord CData [content])
(defrecord Comment [content])

(defn element*
  "Create an xml element from a content collection and optional metadata"
  ([tag attrs content meta]
   (Element. tag (or attrs {}) (remove nil? content) meta))
  ([tag attrs content]
   (Element. tag (or attrs {}) (remove nil? content) nil)))

;; Compiler macro for inlining the two constructors
(alter-meta! #'element* assoc :inline
             (fn
               ([tag attrs content meta]
                `(Element. ~tag (or ~attrs {}) (remove nil? ~content) ~meta))
               ([tag attrs content]
                `(Element. ~tag (or ~attrs {}) (remove nil? ~content) nil))))

(defn element
  "Create an xml Element from content varargs"
  ([tag] (element* tag nil nil))
  ([tag attrs] (element* tag attrs nil))
  ([tag attrs & content] (element* tag attrs content)))

(defn cdata
  "Create a CData node"
  [content]
  (CData. content))

(defn xml-comment
  "Create a Comment node"
  [content]
  (Comment. content))

(defn map->Element [{:keys [tag attrs content] :as el}]
  (element* tag attrs content (meta el)))

(defn tagged-element [el]
  (cond (map? el) (map->Element el)
        ;; TODO support hiccup syntax
        :else (throw (ex-info "Unsupported element representation"
                              {:element el}))))

(defn element? [el]
  (and (map? el) (some? (:tag el))))
