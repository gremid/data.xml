(ns gremid.data.xml.node
  (:require [clojure.string :as str]))


(def element-tag?
  (memoize (fn [kw] (not (str/starts-with? (name kw) "-")))))

(defn element?
  [{:keys [tag]}]
  (when tag (element-tag? tag)))

(defn container?
  [node]
  (or (element? node) (some-> node :tag (= :-document))))

(defn document
  [& content]
  {:tag     :-document
   :attrs   {:encoding   nil
             :standalone nil
             :system-id  nil}
   :content content})

(defn element
  ([tag]
   {:tag     tag
    :attrs   {}
    :content (list)})
  ([tag attrs & content]
   {:tag     tag
    :attrs   (or attrs {})
    :content (or content (list))}))

(defn doc-element
  [doc]
  (some->> doc :content (filter element?) first))

(defn dtd
  [s]
  (element :-dtd {} s))

(defn cdata
  [s]
  (element :-cdata {} s))

(defn pi
  [target data]
  (element :-pi {:target target :data data}))

(defn xml-comment
  [s]
  (element :-comment {} s))

