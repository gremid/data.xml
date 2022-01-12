(ns gremid.data.xml.pprint-test
  "Tests for emit to print XML text."
  (:require
   [gremid.data.xml :refer [indent-str parse-str]]
   [clojure.test :refer [deftest is]]))

(def xml
  "<foo><bar/></foo>")

(def indented-xml
  (str
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "\n"
   "<foo>
  <bar/>
</foo>
"))

(deftest test-indent
  (is (= indented-xml (indent-str (parse-str xml)))))
