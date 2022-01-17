(ns gremid.data.xml.entities-test
  "Test that external entities are not resolved by default, see
  https://www.owasp.org/index.php/XML_External_Entity_(XXE)_Processing"
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [gremid.data.xml :as dx]
   [gremid.data.xml.io :as dx.io])
  (:import
   (javax.xml.stream XMLInputFactory)))

(def vulnerable-input
  "Creates an XML with an external entity referring to the given URL"
  (str "<?xml version=\"1.0\"?>"
       "<!DOCTYPE foo ["
       "  <!ELEMENT foo ANY >"
       "  <!ENTITY xxe SYSTEM \"" (io/resource "secret.txt")  "\" >]>"
       "<foo>&xxe;</foo>"))


(deftest prevent-xxe-by-default
  (testing "To prevent XXE attacks, exernal entities by default do not resolve"
    (is (nil? (re-find #"root_password"
                       (-> vulnerable-input dx/parse dx/emit-str))))))

(def insecure-input-factory
  (doto (dx.io/new-input-factory)
    (.setProperty XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES true)
    (.setProperty XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES true)))

(deftest allow-external-entities-if-required
  (testing "If explicitly enabled, external entities are property resolved"
    (is (some?
         (re-find #"root_password"
                  (->> vulnerable-input
                       (dx/parse insecure-input-factory)
                       (dx/emit-str)))))))
