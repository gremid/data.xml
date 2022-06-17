(ns gremid.data.xml.schematron
  (:require
   [clojure.string :as str]
   [gremid.data.xml.saxon :as dx.saxon]
   [clojure.java.io :as io])
  (:import
   (java.io File)
   (net.sf.saxon.s9api XdmNode)))

(defn resource->xslt
  "Compile XSLT stylesheet from classpath resource."
  [r]
  (dx.saxon/->xslt (.toURI (io/resource r))))

(def rng->sch
  "Extract Schematron rules from RELAX NG stylesheet."
  (resource->xslt "gremid/data/xml/schematron/ExtractSchFromRNG-2.xsl"))

(def sch->sch-xslt
  "Compile Schematron rules into XSLT stylesheet."
  (resource->xslt "gremid/data/xml/schematron/iso_svrl_for_xslt2.xsl"))

(defn extract-xslt
  "Derives a validating Schematron XSLT from a RELAX NG schema with embedded rules."
  [rng-source xslt-destination]
  (let [^File sch (File/createTempFile "gremid.data.xml.schematron." ".sch")]
    (try
      (dx.saxon/transform rng->sch rng-source sch)
      (dx.saxon/transform sch->sch-xslt sch xslt-destination)
      (finally
        (.delete sch)))))

(def xpath-compiler
  (dx.saxon/xpath-compiler {"svrl" "http://purl.oclc.org/dsdl/svrl"}))

(defn- xp-str
  "Creates a string-extracting function based on a XPath."
  [xpath-compiler xp]
  (comp str/join (dx.saxon/selector xpath-compiler  xp)))

(def xp-failures
  "Select Schematron failures."
  (dx.saxon/selector xpath-compiler ".//svrl:failed-assert"))

(def xp-failure-loc
  "Select Schematron failure locations."
  (xp-str xpath-compiler "string(@location)"))

(def xp-failure-text
  "Select Schematron failure messages."
  (xp-str xpath-compiler "svrl:text/text()"))

(defn ->error
  "Convert a Schematron failure to an error record."
  [doc failure]
  (let [location      (xp-failure-loc failure)
        selector      (dx.saxon/selector xpath-compiler location)
        ^XdmNode node (-> doc selector first)]
    {:line    (.getLineNumber node)
     :column  (.getColumnNumber node)
     :message (xp-failure-text failure)}))

(defn validator
  "Creates a validation function based on a Schematron XSL stylesheet."
  [sch-xsl]
  (let [schematron (dx.saxon/->xslt sch-xsl)]
    (fn [source]
      (let [doc          (dx.saxon/->xdm source)
            sch-report   (dx.saxon/transform schematron doc)
            sch-failures (xp-failures sch-report)
            sch-errors   (map (partial ->error doc) sch-failures)]
        sch-errors))))
