(ns gremid.data.xml.saxon
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [gremid.data.xml.io :as dx.io])
  (:import
   (java.io File InputStream OutputStream Reader StringWriter Writer)
   (java.net URI URL)
   (javax.xml.transform Source URIResolver)
   (javax.xml.transform.stream StreamSource)
   (net.sf.saxon Configuration)
   (net.sf.saxon.s9api DocumentBuilder Processor Serializer XdmDestination XdmValue XPathCompiler XPathExecutable XsltCompiler XsltExecutable)
   (org.w3c.dom Node NodeList)))

(defn resolve-uri
  "Resolves URIs, with support for the jar URL scheme."
  ^URI [^URI base ^URI uri]
  (if (= "jar" (.. base (getScheme)))
    (let [[base-jar base-path] (str/split (str base) #"!")
          resolved             (.. (URI. base-path) (resolve uri))]
      (if-not (.isAbsolute resolved) (URI. (str base-jar "!" resolved)) resolved))
    (.resolve base uri)))

(def ^URIResolver uri-resolver
  "A URI resolver with support for resources from JARs on the classpath"
  (proxy [URIResolver] []
    (resolve [^String href ^String base]
      (let [base (URI. (or (not-empty base) ""))
            href (URI. (or (not-empty href) ""))]
        (StreamSource. (str (resolve-uri base href)))))))

(def ^Configuration configuration
  (doto (Configuration.)
    (.setURIResolver uri-resolver)))

(def ^Processor processor
  (Processor. configuration))

(def ^DocumentBuilder doc-builder
  (doto (.newDocumentBuilder processor)
    (.setLineNumbering true)))

(def ^XsltCompiler xslt-compiler
  (.newXsltCompiler processor))

(defn xpath-compiler
  ^XPathCompiler  [ns-decls]
  (let [xpath-compiler (.newXPathCompiler processor)]
    (doseq [[prefix uri] ns-decls]
      (.declareNamespace xpath-compiler prefix uri))
    xpath-compiler))

(defprotocol ApiBridge
  (->xdm [v])
  (->serializer [v])
  (->seq [v])
  (->str [v]))

(extend-protocol ApiBridge
  Source
  (->xdm [v] (.build doc-builder v))

  URI
  (->xdm [v] (->xdm (dx.io/as-source v)))

  URL
  (->xdm [v] (->xdm (dx.io/as-source v)))

  File
  (->xdm [v] (.build doc-builder v))
  (->serializer [v] (.newSerializer processor v))

  InputStream
  (->xdm [v] (->xdm (dx.io/as-source v)))

  OutputStream
  (->serializer [v] (.newSerializer processor v))

  Reader
  (->xdm [v] (->xdm (dx.io/as-source v)))

  Writer
  (->serializer [v] (.newSerializer processor v))

  String
  (->xdm [v] (->xdm (dx.io/as-source v)))
  (->serializer [v] (->serializer (io/file v)))

  NodeList
  (->xdm [v] (.wrap doc-builder v))
  (->seq [v] (->seq (->xdm v)))

  Node
  (->xdm [v] (.wrap doc-builder v))
  (->seq [v] (->seq (->xdm v)))

  XdmValue
  (->xdm [v] v)
  (->seq [v] v)
  (->str [v] (.getStringValue v))

  Serializer
  (->serializer [v] v))

(defn ->xslt
  ^XsltExecutable  [stylesheet]
  (.compile xslt-compiler (dx.io/as-source stylesheet)))

(defn  ->xpath
  ^XPathExecutable [xpath-compiler ^String s]
  (.compile xpath-compiler s))

(defn serialize
  ([source destination]
   (.serializeNode (->serializer destination) (->xdm source)))
  ([source]
   (let [writer (StringWriter.)]
     (serialize source writer)
     (str writer))))

(defn transform
  ([^XsltExecutable stylesheet source]
   (let [destination (XdmDestination.)]
     (transform stylesheet source destination)
     (.getXdmNode destination)))
  ([^XsltExecutable stylesheet source destination]
   (let [source      (dx.io/as-source source)
         destination (->serializer destination)]
     (.. stylesheet (load30) (transform source destination)))))

(defn select
  ^XdmValue [^XPathExecutable xp ctx]
  (.. (doto (.load xp) (.setContextItem (->xdm ctx))) (evaluate)))

(defn selector
  [xpath-compiler ^String s]
  (partial select (->xpath xpath-compiler s)))

