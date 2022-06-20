(ns gremid.data.xml.rng
  (:require
   [gremid.data.xml.io :as dx.io]
   [gremid.data.xml.uri :as dx.uri])
  (:import
   (javax.xml XMLConstants)
   (javax.xml.validation SchemaFactory)
   (org.xml.sax ErrorHandler SAXParseException)))

(defn configure-jing-schema-factory!
  []
  (System/setProperty
   (str (.getName SchemaFactory) ":" XMLConstants/RELAXNG_NS_URI)
   "com.thaiopensource.relaxng.jaxp.XMLSyntaxSchemaFactory"))

(def schema-factory
  (delay
    (->
     (try
       (SchemaFactory/newInstance XMLConstants/RELAXNG_NS_URI)
       (catch IllegalArgumentException _
         (configure-jing-schema-factory!)
         (SchemaFactory/newInstance XMLConstants/RELAXNG_NS_URI)))
     (doto (.setResourceResolver dx.uri/resolver)))))

(defn ->schema
  [rng]
  (.newSchema @schema-factory (dx.io/as-source rng)))

(defn ->error
  "Convert a RELAX NG error to an error record map."
  [severity ^SAXParseException e]
  {:severity severity
   :line     (.getLineNumber e)
   :column   (.getColumnNumber e)
   :message  (.getMessage e)})

(defn validate
  [schema source]
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
