(ns gremid.data.xml.pprint-test
  "Tests for emit to print XML text."
  (:require
   [clojure.test :refer [deftest is]]
   [gremid.data.xml :as dx]))

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
  (is (= indented-xml (dx/indent-str (dx/parse xml)))))

