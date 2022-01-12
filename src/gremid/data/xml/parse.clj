(ns gremid.data.xml.parse
  (:require
   [gremid.data.xml.name :refer [qname]]
   [gremid.data.xml.pu-map :as pu])
  (:import
   (java.io InputStream Reader)
   (javax.xml.stream XMLInputFactory XMLStreamReader)
   (org.codehaus.stax2 XMLInputFactory2)))

(def ^{:private true} input-factory-props
  {:allocator                    XMLInputFactory/ALLOCATOR
   :coalescing                   XMLInputFactory/IS_COALESCING
   :namespace-aware              XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references  XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating                   XMLInputFactory/IS_VALIDATING
   :reporter                     XMLInputFactory/REPORTER
   :resolver                     XMLInputFactory/RESOLVER
   :support-dtd                  XMLInputFactory/SUPPORT_DTD})

(defn- attr-hash
  [^XMLStreamReader sreader]
  (persistent!
   (reduce (fn [tr i]
             (assoc! tr (qname (.getAttributeNamespace sreader i)
                               (.getAttributeLocalName sreader i)
                               (.getAttributePrefix sreader i))
                     (.getAttributeValue sreader i)))
           (transient {})
           (range (.getAttributeCount sreader)))))

(defn- nss-hash
  [^XMLStreamReader sreader parent-hash]
  (pu/persistent!
   (reduce (fn [tr ^long i]
             (pu/assoc! tr
                        (.getNamespacePrefix sreader i)
                        (.getNamespaceURI ^XMLStreamReader sreader i)))
           (pu/transient parent-hash)
           (range (.getNamespaceCount sreader)))))

(defn- location-hash
  [^XMLStreamReader sreader]
  (when-let [location (.getLocation sreader)]
    {:character-offset (.getCharacterOffset location)
     :column-number    (.getColumnNumber location)
     :line-number      (.getLineNumber location)}))

(defn pull-seq'
  "Creates a seq of events."
  [^XMLStreamReader sreader {:keys [include-node? location-info skip-whitespace] :as opts} ns-envs]
  ;; Note, sreader is mutable and mutated here in pull-seq, but it's protected
  ;; by a lazy-seq so it's thread-safe.
  (lazy-seq
   (loop []
     (let [location (when location-info (location-hash sreader))]
       (condp = (and (.hasNext sreader) (.next sreader))
         XMLStreamReader/START_ELEMENT
         (if (include-node? :element)
           (let [ns-env (nss-hash sreader (or (first ns-envs) pu/EMPTY))]
             (cons
              (with-meta
                {:tag   (qname (.getNamespaceURI sreader)
                               (.getLocalName sreader)
                               (.getPrefix sreader))
                 :attrs (attr-hash sreader)}
                {:gremid.data.xml/event         :start
                 :gremid.data.xml/location-info location
                 :gremid.data.xml/nss           ns-env})
              (pull-seq' sreader opts (cons ns-env ns-envs))))
           (recur))
         XMLStreamReader/END_ELEMENT
         (if (include-node? :element)
           (cons
            (with-meta
              {}
              {:gremid.data.xml/event         :end
               :gremid.data.xml/location-info location})
            (pull-seq' sreader opts (rest ns-envs)))
           (recur))
         XMLStreamReader/CHARACTERS
         (if-let [text (and (include-node? :characters)
                            (not (and skip-whitespace (.isWhiteSpace sreader)))
                            (.getText sreader))]
           (if (zero? (.length ^CharSequence text))
             (recur)
             (cons
              (with-meta
                {:content [text]}
                {:gremid.data.xml/event         :chars
                 :gremid.data.xml/location-info location})
              (pull-seq' sreader opts ns-envs)))
           (recur))
         XMLStreamReader/SPACE
         (if-let [text (and (include-node? :characters)
                            (not (and skip-whitespace (.isWhiteSpace sreader)))
                            (.getText sreader))]
           (if (zero? (.length ^CharSequence text))
             (recur)
             (cons
              (with-meta
                {:content [text]}
                {:gremid.data.xml/event         :chars
                 :gremid.data.xml/location-info location})
              (pull-seq' sreader opts ns-envs)))
           (recur))
         XMLStreamReader/COMMENT
         (if (include-node? :comment)
           (cons
            (with-meta
              {:tag     :-comment
               :content [(.getText sreader)]}
              {:gremid.data.xml/event         :comment
               :gremid.data.xml/location-info location})
            (pull-seq' sreader opts ns-envs))
           (recur))
         XMLStreamReader/PROCESSING_INSTRUCTION
         (if (include-node? :pi)
           (cons
            (with-meta
              {:tag     :-pi
               :attrs   {:target (.getPITarget sreader)
                         :data   (.getPIData sreader)}
               :content (list)}
              {:gremid.data.xml/event         :pi
               :gremid.data.xml/location-info location})
            (pull-seq' sreader opts ns-envs))
           (recur))
         ;; end of stream
         false
         nil
         ;; Consume and ignore comments, spaces, processing instructions etc
         (recur))))))

(defn pull-seq
  [^XMLStreamReader sreader {:keys [include-node?] :as opts} ns-envs]
  (let [document? (or (include-node? :document) (include-node? :pi))
        evt-seq   (pull-seq' sreader opts ns-envs)]
    (if document?
      (concat [(with-meta
                 {:tag     :-document
                  :attrs   {}
                  :content (list)}
                 {:gremid.data.xml/event :start})]
              evt-seq
              [(with-meta
                 {}
                 {:gremid.data.xml/event :end})])
      evt-seq)))

(defn- ^XMLInputFactory2 make-input-factory
  [props]
  (let [^XMLInputFactory2 fac (XMLInputFactory/newInstance)]
    (doto fac
      (.configureForRoundTripping)
      (.setProperty XMLInputFactory2/P_REPORT_PROLOG_WHITESPACE true))
    (doseq [[k v] props
            :when (contains? input-factory-props k)
            :let  [prop (input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn make-stream-reader
  [props source]
  (let [fac (make-input-factory props)]
    (cond
      (instance? Reader source)      (.createXMLStreamReader fac ^Reader source)
      (instance? InputStream source) (.createXMLStreamReader fac ^InputStream source)
      :else                          (throw
                                      (IllegalArgumentException.
                                       (str "source should be java.io.Reader "
                                            "or java.io.InputStream"))))))

(defn string-source [s]
  (java.io.StringReader. s))
