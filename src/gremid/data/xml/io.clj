(ns gremid.data.xml.io
  (:import
   (com.ctc.wstx.api WstxOutputProperties)
   (java.io File InputStream OutputStream Reader StringReader Writer)
   (java.net URI URL)
   (javax.xml.parsers DocumentBuilder DocumentBuilderFactory)
   (javax.xml.stream XMLEventReader XMLInputFactory XMLOutputFactory)
   (javax.xml.transform OutputKeys Result Source Transformer TransformerFactory)
   (javax.xml.transform.dom DOMResult DOMSource)
   (javax.xml.transform.stream StreamResult StreamSource)
   (org.codehaus.stax2 XMLInputFactory2 XMLOutputFactory2)
   (org.w3c.dom Node)
   (org.xml.sax InputSource)))

(defprotocol AsSource
  (as-source ^Source [v])
  (as-input-source [v]))

(extend-protocol AsSource
  Source
  (as-source [^Source v] v)

  Node
  (as-source [^Node v] (DOMSource. v))

  File
  (as-source [^File v] (StreamSource. v))
  (as-input-source [^File v] (as-input-source (.toURI v)))

  URI
  (as-source [^URI v] (StreamSource. (str v)))
  (as-input-source [^URI v] (InputSource. (str v)))

  URL
  (as-source [^URL v] (as-source (.toURI v)))
  (as-input-source [^URL v] (as-input-source (.toURI v)))

  InputStream
  (as-source [^InputStream v] (StreamSource. v))
  (as-input-source [^InputStream v] (InputSource. v))

  Reader
  (as-source [^Reader v] (StreamSource. v))
  (as-input-source [^Reader v] (InputSource. v))

  String
  (as-source [^String v] (as-source (StringReader. v)))
  (as-input-source [^String v] (as-input-source (StringReader. v))))

(defn new-input-factory
  (^XMLInputFactory2 []
   (XMLInputFactory2/newInstance)))

(def round-tripping-input-factory
  (doto (new-input-factory) (.configureForRoundTripping)))

(defn event-reader
  (^XMLEventReader [^InputStream input]
   (event-reader round-tripping-input-factory input))
  (^XMLEventReader [^XMLInputFactory factory input]
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
  (doto ^XMLOutputFactory2 (XMLOutputFactory2/newInstance)
    (.configureForXmlConformance)
    (.setProperty WstxOutputProperties/P_USE_DOUBLE_QUOTES_IN_XML_DECL true)))

(defn event-writer
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

(defn indenting-transformer
  (^Transformer []
   (doto (.newTransformer transformer-factory)
     (.setOutputProperty OutputKeys/INDENT "yes")
     (.setOutputProperty OutputKeys/METHOD "xml")
     ;; print newline after preamble
     (.setOutputProperty OutputKeys/DOCTYPE_PUBLIC "yes"))))

(defn indent
  [input output]
  (.transform (indenting-transformer) (as-source input) (as-result output)))
