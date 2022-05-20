(ns gremid.data.xml.name
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log])
  (:import
   (clojure.lang Keyword Namespace)
   (java.net URLDecoder URLEncoder)
   (javax.xml.namespace QName)
   (javax.xml.stream.events XMLEvent)))

(def xmlns-uri
  "http://www.w3.org/2000/xmlns/")

(def xml-uri
  "http://www.w3.org/XML/1998/namespace")

(def initial-ns-ctx
  {"xml"   xml-uri
   "xmlns" xmlns-uri})

(defn legal-xmlns-binding!
  [prefix uri]
  (when (not= (= "xml" prefix) (= xml-uri uri))
    (throw (ex-info
            (str "The xmlns binding for prefix `xml` is fixed to `" xml-uri "`")
            {:attempted-mapping {:prefix prefix :uri uri}})))
  (when (not= (= "xmlns" prefix) (= xmlns-uri uri))
    (throw (ex-info
            (str "The xmlns binding for prefix `xmlns` is fixed to `" xmlns-uri "`")
            {:attempted-mapping {:prefix prefix :uri uri}}))))

(def ^QName parse-qname
  (memoize (fn [s] (QName/valueOf s))))

(defprotocol AsQName
  (qname-local [qname] "Get the name for this qname")
  (qname-uri   [qname] "Get the namespace uri for this qname"))

(extend-protocol AsQName
  QName
  (qname-local [qname] (.getLocalPart qname))
  (qname-uri   [qname] (.getNamespaceURI qname)))

(extend-protocol AsQName
  String
  (qname-local [s] (.getLocalPart (parse-qname s)))
  (qname-uri   [s] (.getNamespaceURI (parse-qname s))))

(defn decode-uri
  [ns]
  (URLDecoder/decode ns "UTF-8"))

(defn encode-uri
  [uri]
  (URLEncoder/encode uri "UTF-8"))

(defn uri-symbol [uri]
  (symbol (encode-uri (str "xmlns." uri))))

(defn symbol-uri [ss]
  (let [du (decode-uri (str ss))]
    (if (.startsWith du "xmlns.")
      (subs du 6)
      (throw (ex-info "Uri symbol not valid" {:sym ss})))))

(defn qname
  ([local]
   (qname "" local))
  ([uri local]
   (keyword (when-not (str/blank? uri) (encode-uri (str "xmlns." uri))) local))
  ([uri local _]
   (qname uri local)))

;; The empty string shall be equal to nil for xml names
(defn namespaced?
  [qn]
  (not (str/blank? (qname-uri qn))))

(defn clj-ns-name
  [ns]
  (cond
    (instance? Namespace ns) (ns-name ns)
    (keyword? ns)            (name ns)
    :else                    (str ns)))

(extend-protocol AsQName
  Keyword
  (qname-local [kw] (name kw))
  (qname-uri [kw]
    (if-let [ns (namespace kw)]
      (if (.startsWith ns "xmlns.")
        (decode-uri (subs ns 6))
        (case ns
          "xmlns" xmlns-uri
          "xml"   xml-uri
          (throw (ex-info "Keyword ns is not an xmlns. Needs to be in the form :xmlns.<encoded-uri>/<local>" {:kw kw}))))
      "")))

(defn as-qname
  [n]
  (qname (qname-uri n) (qname-local n)))

(defn xmlns-attr?
  "Is this qname an xmlns declaration?"
  [qn]
  (let [uri (qname-uri qn)]
    (or (= xmlns-uri uri)
        (and (str/blank? uri)
             (= "xmlns" (qname-local qn))))))

(defn xmlns-attr-prefix
  [qn]
  (let [uri (qname-uri qn)]
    (if (str/blank? uri)
      (do (when-not (= "xmlns" (qname-local qn))
            (throw (ex-info "Not an xmlns-attr name" {:qname qn})))
          "")
      (do (when-not (= xmlns-uri uri)
            (throw (ex-info "Not an xmlns-attr name" {:qname qn})))
          (qname-local qn)))))

(defn separate-xmlns
  "Separates xmlns attributes from other attributes"
  [attrs]
  (loop [attrs*          (transient {})
         xmlns*          (transient {})
         [qn :as attrs'] (keys attrs)]
    (if (seq attrs')
      (let [val (get attrs qn)]
        (if (xmlns-attr? qn)
          (let [prefix (xmlns-attr-prefix qn)]
            (legal-xmlns-binding! prefix val)
            (recur attrs*
                   (assoc! xmlns* prefix val)
                   (next attrs')))
          (recur (assoc! attrs* qn val)
                 xmlns*
                 (next attrs'))))
      [(persistent! xmlns*) (persistent! attrs*)])))

;; # Namespace prefix/URI mapping

(defn assoc'
  [ns-ctx prefix uri]
  (legal-xmlns-binding! prefix uri)
  (if (str/blank? uri) (dissoc ns-ctx prefix) (assoc ns-ctx prefix uri)))

(defn get-prefixes
  [ns-ctx uri]
  (seq (for [[p uri'] ns-ctx :when (= uri uri')] p)))

(defn get-prefix
  [ns-ctx uri]
  (first (get-prefixes ns-ctx uri)))

(defn diff
  [ns-ctx1 ns-ctx2]
  (concat
   (map (fn [[p _]] [p ""]) (remove (comp ns-ctx2 first) ns-ctx1))
   (remove (fn [[p u]] (= u (ns-ctx1 p))) ns-ctx2)))

(def ^:private ^"[C" prefix-alphabet
  (char-array (map char (range (int \a) (inc (int \z))))))

(def ^:dynamic *gen-prefix-counter*
  "Thread local counter for a single document"
  0)

(defn gen-prefix'
  "Generates an xml prefix. Zero-arity can only be called, when
  *gen-prefix-counter* is bound and will increment it."
  ([]
   (let [c *gen-prefix-counter*]
     (set! *gen-prefix-counter* (inc c))
     (gen-prefix' c)))
  ([n]
   (let [cnt (alength prefix-alphabet)
         sb  (StringBuilder.)]
     (loop [n* n]
       (let [ch  (mod n* cnt)
             n** (quot n* cnt)]
         (.append sb (aget prefix-alphabet ch))
         (if (pos? n**)
           (recur n**)
           (str sb)))))))

(defn gen-prefix
  [ns-ctx uri suggested]
  (or (get-prefix ns-ctx uri)
      (loop [prefix (or suggested (gen-prefix'))]
        (if (get ns-ctx prefix)
          (recur (gen-prefix'))
          prefix))))

(defn compute
  [ns-ctx {:keys [tag] :as node} xmlns-attrs attrs]
  (let [el-ns-ctx (reduce
                   (fn [ns-ctx [prefix uri]] (assoc' ns-ctx prefix uri))
                   (get (meta node) :gremid.data.xml/ns-ctx initial-ns-ctx)
                   xmlns-attrs)
        ;; add namespaces from current environment
        ns-ctx    (reduce
                   (fn [ns-ctx [ns-attr uri]]
                     (assoc' ns-ctx (gen-prefix ns-ctx uri ns-attr) uri))
                   ns-ctx
                   el-ns-ctx)
        ;; add implicit namespaces used by tag, attrs
        uri       (qname-uri tag)
        local     (qname-local tag)
        attr-uris (map qname-uri (keys attrs))
        ns-ctx    (reduce
                   (fn [ns-ctx uri] (assoc' ns-ctx (gen-prefix ns-ctx uri nil) uri))
                   ns-ctx
                   (cond->> attr-uris (not-empty uri) (cons uri)))]
    ;; rename default namespace, if tag is global (not in a namespace)
    (if-let [uri (and (str/blank? uri) (get ns-ctx ""))]
      (let [ns-ctx (assoc' ns-ctx "" "")
            ns-ctx (assoc' ns-ctx (gen-prefix ns-ctx uri nil) uri)]
        (log/tracef (str "Default `xmlns=\"%s\"` had to be replaced "
                         "with a `xmlns=\"\"` because of global "
                         "element `%s`")
                    uri local)
        ns-ctx)
      ns-ctx)))

(defn child-ns-ctx
  [ns-ctx ^XMLEvent event]
  (reduce
   (fn [ns-ctx ^javax.xml.stream.events.Namespace ns]
     (assoc' ns-ctx (.getPrefix ns) (.getNamespaceURI ns)))
   ns-ctx
   (when (.isStartElement event) (iterator-seq (.getNamespaces event)))))
