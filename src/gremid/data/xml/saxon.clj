(ns gremid.data.xml.saxon
  (:require
   [clojure.java.io :as io]
   [gremid.data.xml.io :as dx.io]
   [gremid.data.xml.uri :as dx.uri])
  (:import
   (java.io File InputStream OutputStream Reader StringWriter Writer)
   (java.net URI URL)
   (javax.xml.transform Source URIResolver)
   (net.sf.saxon Configuration)
   (net.sf.saxon.s9api DocumentBuilder Processor Serializer XdmDestination XdmNode XdmValue XPathCompiler XPathExecutable XsltCompiler XsltExecutable)
   (org.w3c.dom Node NodeList)))

(def ^Configuration configuration
  (doto (Configuration.)
    (.setURIResolver ^URIResolver dx.uri/resolver)))

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
  (->source [v])
  (->xdm [v])
  (->serializer [v])
  (->seq [v])
  (->str [v]))

(extend-protocol ApiBridge
  Source
  (->source [v] v)
  (->xdm [v] (.build doc-builder v))

  URI
  (->source [v] (dx.io/as-source v))
  (->xdm [v] (->xdm (->source v)))

  URL
  (->source [v] (dx.io/as-source v))
  (->xdm [v] (->xdm (->source v)))

  File
  (->source [v] (dx.io/as-source v))
  (->xdm [v] (.build doc-builder v))
  (->serializer [v] (.newSerializer processor v))

  InputStream
  (->source [v] (dx.io/as-source v))
  (->xdm [v] (->xdm (->source v)))

  OutputStream
  (->serializer [v] (.newSerializer processor v))

  Reader
  (->source [v] (dx.io/as-source v))
  (->xdm [v] (->xdm (->source v)))

  Writer
  (->serializer [v] (.newSerializer processor v))

  String
  (->source [v] (dx.io/as-source v))
  (->xdm [v] (->xdm (->source v)))
  (->serializer [v] (->serializer (io/file v)))

  NodeList
  (->xdm [v] (.wrap doc-builder v))
  (->seq [v] (->seq (->xdm v)))

  Node
  (->xdm [v] (.wrap doc-builder v))
  (->seq [v] (->seq (->xdm v)))

  XdmNode
  (->source [v] (.asSource v))
  (->xdm [v] v)
  (->seq [v] v)
  (->str [v] (.getStringValue v))

  XdmDestination
  (->serializer [v] v)

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
   (let [source      (->source source)
         destination (->serializer destination)]
     (.. stylesheet (load30) (transform source destination)))))

(defn select
  ^XdmValue [^XPathExecutable xp ctx]
  (.. (doto (.load xp) (.setContextItem (->xdm ctx))) (evaluate)))

(defn selector
  [xpath-compiler ^String s]
  (partial select (->xpath xpath-compiler s)))

