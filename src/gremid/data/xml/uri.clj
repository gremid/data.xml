(ns gremid.data.xml.uri
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.net URI)
   (javax.xml.transform URIResolver)
   (javax.xml.transform.stream StreamSource)
   (org.w3c.dom.ls LSInput LSResourceResolver)))

(defn resolve-uri
  "Resolves URIs, with support for the jar URL scheme."
  ^URI [^URI base ^URI uri]
  (if (= "jar" (.. base (getScheme)))
    (let [[base-jar base-path] (str/split (str base) #"!")
          resolved             (.. (URI. base-path) (resolve uri))]
      (if-not (.isAbsolute resolved) (URI. (str base-jar "!" resolved)) resolved))
    (.resolve base uri)))

(def resolver
  "A resolver with support for resources from JARs on the classpath"
  (proxy [URIResolver LSResourceResolver] []
    (resolve [^String href ^String base]
      (let [base (URI. (or (not-empty base) ""))
            href (URI. (or (not-empty href) ""))]
        (StreamSource. (str (resolve-uri base href)))))
    (resolveResource [_ _ _ ^String href ^String base]
      (when href
        (let [base (URI. (or (not-empty base) ""))
              href (URI. (or (not-empty href) ""))
              uri  (resolve-uri base href)]
          (proxy [LSInput] []
            (getSystemId [] (str uri))
            (getByteStream [] (io/input-stream uri))
            (getEncoding [])
            (getStringData [])
            (getCharacterStream [])
            (getPublicId [])
            (getBaseURI [])))))))

