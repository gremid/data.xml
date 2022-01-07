(ns clojure.data.xml.spec
  (:require
   [clojure.data.xml :as xml]
   [clojure.data.xml.name :as name]
   [clojure.data.xml.node :as node]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as str]))

(s/def ::qname-conformer
  (s/and (s/conformer
          (fn [qn]
            (try {:uri (name/qname-uri qn)
                  :local (name/qname-local qn)}
                 (catch Throwable t
                   (println "Could not conform to qname:" qn)
                   (.printStackTrace t)
                   ::s/invalid)))
          (fn [{:keys [uri local] :as arg}]
            (println arg)
            (name/qname uri local)))
         #(not (str/blank? (:local %)))))

(s/def ::name/qname
  (->
   (s/or
    :global (s/or :kw (s/and simple-keyword? ::qname-conformer)
                  :str (s/and string? ::qname-conformer #(str/blank? (:uri %))))
    :qualified (s/or :kw (s/and qualified-keyword? ::qname-conformer)
                     :str (s/and string? ::qname-conformer)))
   (s/with-gen
     #(gen/fmap (fn [[uri local]]
                  (name/qname uri local))
                (gen/tuple (gen/fmap (fn [s] (when-not (str/blank? s)
                                               (str "urn:" s)))
                                     (gen/string-alphanumeric))
                           (gen/fmap (partial str "G") (gen/string-alphanumeric)))))))


(s/def ::node/Element
  (s/keys :req-un [::node/tag]
          :opt-un [::node/attrs ::node/content]))

(s/def ::xml/Element
  ::node/Element)

(s/def ::xml/Text
  (s/or :blank (s/with-gen str/blank? #(s/gen #{"" nil}))
        :str string?))

(s/def ::xml/Node
  (s/or :text ::xml/Text
        :elem ::xml/Element))

(s/def ::node/tag
  ::name/qname)

(s/def ::node/attrs
  (s/map-of ::name/qname string?
            :conform-keys true))

(s/def ::node/content
  (s/coll-of ::xml/Node))

(comment
  (s/conform (s/coll-of ::name/qname) [:foo :xmlns/foo])
  (s/exercise ::name/qname))
