(ns gremid.data.xml.rng
  (:require
   [gremid.data.xml.io :as dx.io]
   [gremid.data.xml.uri :as dx.uri])
  (:import
   (javax.xml XMLConstants)
   (javax.xml.transform Source)
   (javax.xml.validation Schema SchemaFactory)
   (org.xml.sax ErrorHandler SAXParseException)))

(defn configure-jing-schema-factory!
  []
  (System/setProperty
   (str (.getName SchemaFactory) ":" XMLConstants/RELAXNG_NS_URI)
   "com.thaiopensource.relaxng.jaxp.XMLSyntaxSchemaFactory"))

(def schema-factory
  (delay
    (let [^SchemaFactory sf
          (try
            (SchemaFactory/newInstance XMLConstants/RELAXNG_NS_URI)
            (catch IllegalArgumentException _
              (configure-jing-schema-factory!)
              (SchemaFactory/newInstance XMLConstants/RELAXNG_NS_URI)))]
      (doto sf
        (.setResourceResolver dx.uri/resolver)))))

(defn ->schema
  [rng]
  (.newSchema ^SchemaFactory @schema-factory ^Source (dx.io/as-source rng)))

(defn ->error
  "Convert a RELAX NG error to an error record map."
  [severity ^SAXParseException e]
  {:severity severity
   :line     (.getLineNumber e)
   :column   (.getColumnNumber e)
   :message  (.getMessage e)})

(defn validate
  [^Schema schema source]
  (let [validator     (.newValidator schema)
        errors        (transient [])
        add-error     #(conj! errors (->error %1 %2))
        error-handler (proxy [ErrorHandler] []
                        (error [e] (add-error :error e))
                        (fatalError [e] (add-error :fatal e))
                        (warning [e] (add-error :warning e)))]
    (.setErrorHandler validator error-handler)
    (.validate validator (dx.io/as-source source))
    (persistent! errors)))
