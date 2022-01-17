(ns gremid.data.xml.util
  "Tests for emit to print XML text."
  (:require
   [gremid.data.xml :as dx])
  (:import
   (java.io ByteArrayInputStream)))

(defn test-stream
  [x]
  (ByteArrayInputStream. (.getBytes x "UTF-8")))

(def lazy-parse*
  (comp dx/parse test-stream))

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

(defn doc-element
  [doc]
  (some->> doc :content (filter :tag) first))
