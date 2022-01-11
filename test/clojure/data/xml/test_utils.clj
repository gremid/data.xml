(ns clojure.data.xml.test-utils
  "Tests for emit to print XML text."
  (:require
   [clojure.data.xml :as xml :refer [parse]])
  (:import
   (java.io ByteArrayInputStream)))

(defn test-stream
  [x]
  (ByteArrayInputStream. (.getBytes x "UTF-8")))

(def lazy-parse*
  (comp parse test-stream))

(defn element
  ([tag]
   {:tag     tag
    :attrs   {}
    :content (list)})
  ([tag attrs & content]
   {:tag     tag
    :attrs   (or attrs {})
    :content (or content (list))}))

(defn cdata
  [s]
  (element :-cdata {} s))

(defn xml-comment
  [s]
  (element :-comment {} s))
