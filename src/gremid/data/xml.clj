(ns gremid.data.xml
  "Functions to parse XML into lazy sequences and lazy trees and emit these as
  text."
  (:require
   [gremid.data.xml.io :as dx.io]
   [gremid.data.xml.name :as dx.name]
   [gremid.data.xml.sexp :as dx.sexp]
   [gremid.data.xml.stax :as dx.stax]
   [gremid.data.xml.tree :as dx.tree])
  (:import
   (javax.xml.stream XMLEventWriter)
   (javax.xml.stream.events XMLEvent)))

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
      (let [xn (dx.name/uri-symbol n)
            al (symbol (dx.name/clj-ns-name a))]
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
  ([sexp] (dx.sexp/as-elements sexp))
  ([sexp & sexps] (mapcat dx.sexp/as-elements (cons sexp sexps))))

(defn sexp-as-element
  "Convert a single sexp into an Element"
  [sexp]
  (let [[root & more] (sexps-as-fragment sexp)]
    (when more
      (throw
       (IllegalArgumentException.
        "Cannot have multiple root elements; try creating a fragment instead")))
    root))

(defn ->seq
  "Parses an XML input source into a (lazy) seq of XML events."
  ([input]
   (->seq dx.io/round-tripping-input-factory input))
  ([input-factory input]
   (dx.stax/->data
    (iterator-seq
     (dx.io/event-reader input-factory input)))))

(defn parse
  "Parses an XML input source into a a tree of nodes.

  The element tree is realized lazily, so huge XML files can be streamed through
  a depth-first tree walk."
  ([input]
   (parse dx.io/round-tripping-input-factory input))
  ([input-factory input]
   (dx.tree/->tree (->seq input-factory input))))

(defn pull-all
  "Walks the given tree of nodes, eagerly realizing all descendants."
  [node]
  (cond-> node
    (:content node) (update :content #(doall (map pull-all %)))))

(defn stream
  "Streams the given event sequence as XML text to the output."
  ([events output]
   (stream dx.io/conforming-output-factory events output))
  ([output-factory events output]
   (let [^XMLEventWriter ew (dx.io/event-writer output-factory output)]
     (doseq [event (dx.stax/->stax-events events)]
       (.add ew ^XMLEvent event))
     (.flush ew))))

(defn stream-str
  "Streams the given event sequence to an XML text string."
  ([events]
   (with-out-str
     (stream events *out*)))
  ([output-factory events]
   (with-out-str
     (stream output-factory events *out*))))

(defn emit
  "Prints the given node tree as XML text to the output."
  ([node output]
   (emit dx.io/conforming-output-factory node output))
  ([output-factory node output]
   (stream output-factory (dx.tree/->seq node) output)
   node))

(defn emit-str
  "Emits the given node tree as an XML text string."
  ([node]
   (emit-str dx.io/conforming-output-factory node))
  ([output-factory node]
   (stream-str output-factory (dx.tree/->seq node))))

(defn indent
  ([output-factory node output]
   (let [buf (dx.io/new-document)]
     (emit output-factory node buf)
     (dx.io/indent buf output))
   node)
  ([node output]
   (let [buf (dx.io/new-document)]
     (emit node buf)
     (dx.io/indent buf output))
   node))

(defn indent-str
  ([output-factory node]
   (with-out-str
     (indent output-factory node *out*)))
  ([node]
   (with-out-str
     (indent node *out*))))
