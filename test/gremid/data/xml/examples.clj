(ns gremid.data.xml.examples
  (:require [gremid.data.xml :as dx]))

(dx/parse-str "<root/>")
;; => {:tag :root, :attrs {}, :content ()}

(with-open [r (java.io.StringReader. "<a><b>c</b></a>")]
  (dx/parse r))
;; => {:tag :a, :attrs {}, :content ({:tag :b, :attrs {}, :content ("c")})}

(with-open [r (java.io.StringReader. "<a><b>c</b></a>")]
  (dx/emit-str (dx/parse r)))
;; => "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a><b>c</b></a>"

(dx/alias-uri :tei "http://www.tei-c.org/ns/1.0")

(dx/sexp-as-element
 [::tei/TEI
  [::tei/teiHeader]
  [::tei/text
   "Hello World"]])
;; => {:tag :xmlns.http%3A%2F%2Fwww.tei-c.org%2Fns%2F1.0/TEI,
;;     :attrs {},
;;     :content
;;     ({:tag :xmlns.http%3A%2F%2Fwww.tei-c.org%2Fns%2F1.0/teiHeader,
;;       :attrs {},
;;       :content ()}
;;      {:tag :xmlns.http%3A%2F%2Fwww.tei-c.org%2Fns%2F1.0/text,
;;       :attrs {},
;;       :content ("Hello World")})}

(dx/emit-str
 (dx/sexp-as-element
  [::tei/text {:xmlns "http://www.tei-c.org/ns/1.0"}
   [:-comment "CDATA and comments can be emitted"]
   [:-cdata "<--"]]))
;; => "<?xml version=\"1.0\" encoding=\"UTF-8\"?><text xmlns=\"http://www.tei-c.org/ns/1.0\"><!--CDATA and comments can be emitted--><![CDATA[<--]]></text>"

(-> (dx/parse-str "<root/>") (meta) ::dx/location-info)
;; => {:character-offset 0, :column-number 1, :line-number 1}
