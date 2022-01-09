;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Tests for emit to print XML text."
      :author "Chris Houser"}
 clojure.data.xml.test-utils
  (:require
   [clojure.data.xml :as xml :refer [parse]])
  (:import
   (java.io ByteArrayInputStream)))

(defn test-stream
  [x]
  (ByteArrayInputStream. (.getBytes x "UTF-8")))

(def lazy-parse*
  (comp parse test-stream))

(defn element
  ([tag]
   {:tag     tag
    :attrs   {}
    :content (list)})
  ([tag attrs & content]
   {:tag     tag
    :attrs   (or attrs {})
    :content (or content (list))}))

(defn cdata
  [s]
  (element :-cdata {} s))

(defn xml-comment
  [s]
  (element :-comment {} s))
