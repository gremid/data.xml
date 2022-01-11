(ns clojure.data.xml.pprint
  (:import
   (java.io StringReader Writer)
   (javax.xml.transform OutputKeys Transformer TransformerFactory)
   (javax.xml.transform.stream StreamResult StreamSource)))

(defn ^Transformer indenting-transformer []
  (doto (-> (TransformerFactory/newInstance) .newTransformer)
    (.setOutputProperty OutputKeys/INDENT "yes")
    (.setOutputProperty OutputKeys/METHOD "xml")
    (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")
    ;; print newline after preamble
    (.setOutputProperty OutputKeys/DOCTYPE_PUBLIC "yes")))

(defn indent-xml
  [s ^Writer writer]
  (let [source (-> s StringReader. StreamSource.)
        result (StreamResult. writer)]
    (.transform (indenting-transformer) source result)))


