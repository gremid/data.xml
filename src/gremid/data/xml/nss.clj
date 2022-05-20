(ns gremid.data.xml.nss
  "Keeping track of prefix->uri mappings in xml namespaces."
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [gremid.data.xml.name :as dx.name])
  (:import
   (javax.xml.stream.events Namespace XMLEvent)))

(def EMPTY
  {"xml"   dx.name/xml-uri
   "xmlns" dx.name/xmlns-uri})

(defn assoc'
  [pm prefix uri]
  (dx.name/legal-xmlns-binding! prefix uri)
  (if (str/blank? uri) (dissoc pm prefix) (assoc pm prefix uri)))

(defn get-prefixes
  [pm uri]
  (seq (for [[p uri'] pm :when (= uri uri')] p)))

(defn get-prefix
  [pm uri]
  (first (get-prefixes pm uri)))

(defn diff
  [pm1 pm2]
  (concat
   (map (fn [[p _]] [p ""]) (remove (comp pm2 first) pm1))
   (remove (fn [[p u]] (= u (pm1 p))) pm2)))

(defn compute-prefix
  [pm uri suggested]
  (or (get-prefix pm uri)
      (loop [prefix (or suggested (dx.name/gen-prefix))]
        (if (get pm prefix)
          (recur (dx.name/gen-prefix))
          prefix))))

(defn compute
  [pm {:keys [tag] :as node} xmlns-attrs attrs]
  (let [el-pm     (reduce
                     (fn [pm [prefix uri]] (assoc' pm prefix uri))
                     (get (meta node) :gremid.data.xml/nss EMPTY)
                     xmlns-attrs)
        ;; add namespaces from current environment
        pm        (reduce
                     (fn [pm [ns-attr uri]]
                       (assoc' pm (compute-prefix pm uri ns-attr) uri))
                     pm
                     el-pm)
        ;; add implicit namespaces used by tag, attrs
        uri       (dx.name/qname-uri tag)
        local     (dx.name/qname-local tag)
        attr-uris (map dx.name/qname-uri (keys attrs))
        pm        (reduce
                     (fn [pm uri] (assoc' pm (compute-prefix pm uri nil) uri))
                     pm
                     (cond->> attr-uris (not-empty uri) (cons uri)))]
    ;; rename default namespace, if tag is global (not in a namespace)
    (if-let [uri (and (str/blank? uri) (get pm ""))]
      (let [pm (assoc' pm "" "")
            pm (assoc' pm (compute-prefix pm uri nil) uri)]
        (log/tracef (str "Default `xmlns=\"%s\"` had to be replaced "
                         "with a `xmlns=\"\"` because of global "
                         "element `%s`")
                    uri local)
        pm)
      pm)))

(defn child-nss
  [nss ^XMLEvent event]
  (reduce
   (fn [nss ^Namespace ns] (assoc' nss (.getPrefix ns) (.getNamespaceURI ns)))
   nss
   (when (.isStartElement event) (iterator-seq (.getNamespaces event)))))
