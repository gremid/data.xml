(ns gremid.data.xml.test-util
  "Tests for emit to print XML text."
  (:require
   [gremid.data.xml :as dx]
   [gremid.data.xml.node :as dx.node])
  (:import
   (java.io ByteArrayInputStream)))


(defn test-stream
  [x]
  (ByteArrayInputStream. (.getBytes x "UTF-8")))

(def parse-str
  (comp dx/parse test-stream))

(defn emit-fragment-str
  [doc-el]
  (dx/emit-str (dx.node/document doc-el)))
