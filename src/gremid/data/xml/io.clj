(ns gremid.data.xml.io
  (:import
   (com.ctc.wstx.api WstxOutputProperties)
   (java.io InputStream OutputStream Reader StringReader Writer)
   (javax.xml.parsers DocumentBuilder DocumentBuilderFactory)
   (javax.xml.stream XMLEventReader XMLEventWriter XMLInputFactory XMLOutputFactory)
   (javax.xml.transform OutputKeys Result Source Transformer TransformerFactory)
   (javax.xml.transform.dom DOMResult DOMSource)
   (javax.xml.transform.stream StreamResult StreamSource)
   (org.codehaus.stax2 XMLInputFactory2 XMLOutputFactory2)
   (org.w3c.dom Node)))

(defprotocol AsSource
  (as-source [v]))

(extend-protocol AsSource
  Source
  (as-source [^Source v] v)

  Node
  (as-source [^Node v] (DOMSource. v))

  InputStream
  (as-source [^InputStream v] (StreamSource. v))

  Reader
  (as-source [^Reader v] (StreamSource. v))

  String
  (as-source [^String v] (as-source (StringReader. v))))

(defn ^XMLInputFactory2 new-input-factory
  []
  (XMLInputFactory2/newInstance))

(def round-tripping-input-factory
  (doto (new-input-factory) (.configureForRoundTripping)))

(defn ^XMLEventReader event-reader
  ([^InputStream input]
   (event-reader round-tripping-input-factory input))
  ([^XMLInputFactory factory input]
   (.createXMLEventReader factory ^Source (as-source input))))

(defprotocol AsResult
  (as-result [v]))

(extend-protocol AsResult
  Result
  (as-result [^Result v] v)

  Node
  (as-result [^Node v] (DOMResult. v))

  OutputStream
  (as-result [^OutputStream v] (StreamResult. v))

  Writer
  (as-result [^Writer v] (StreamResult. v)))

(def conforming-output-factory
  (doto (XMLOutputFactory2/newInstance)
    (.configureForXmlConformance)
    (.setProperty WstxOutputProperties/P_USE_DOUBLE_QUOTES_IN_XML_DECL true)))

(defn ^XMLEventWriter event-writer
  ([output]
   (event-writer conforming-output-factory output))
  ([^XMLOutputFactory factory output]
   (.createXMLEventWriter factory ^Result (as-result output))))

(def ^DocumentBuilderFactory doc-builder-factory
  (DocumentBuilderFactory/newInstance))

(def ^DocumentBuilder doc-builder
  (.newDocumentBuilder doc-builder-factory))

(defn new-document
  []
  (doto (.newDocument doc-builder) (.setXmlStandalone true)))

(def ^TransformerFactory transformer-factory
  (TransformerFactory/newInstance))

(defn ^Transformer indenting-transformer
  []
  (doto (.newTransformer transformer-factory)
    (.setOutputProperty OutputKeys/INDENT "yes")
    (.setOutputProperty OutputKeys/METHOD "xml")
    (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")
    ;; print newline after preamble
    (.setOutputProperty OutputKeys/DOCTYPE_PUBLIC "yes")))

(defn indent
  [input output]
  (.transform (indenting-transformer) (as-source input) (as-result output)))
