# gremid.data.xml – treating XML documents as Clojure data structures

This is a Clojure library for reading and writing XML data. It is a fork of
clojure-contrib's [data.xml](https://github.com/clojure/data.xml) library with a
focus on round-tripping XML documents, including DOM node types not supported by
`data.xml`, i. e. document nodes and processing instructions. Also this fork is
not aiming for API compatibility. While most of the functions in the core
namespace `gremid.data.xml` have the same semantics as those in
`clojure.data.xml`, the internals of the library have been refactored and
ClojureScript support has been dropped.

## Installation

Add the following to the `deps.edn` dependencies:

```clojure
io.github.gremid/data.xml {:git/sha "..."}
```

## Examples

```clojure
(ns gremid.data.xml.examples
  (:require [gremid.data.xml :as dx]))

(dx/parse "<root/>")
;; => {:tag :-document,
;;     :attrs {:encoding nil, :standalone nil, :system-id nil},
;;     :content ({:tag :root, :attrs {}, :content ()})}

(with-open [r (java.io.StringReader. "<a><b>c</b></a>")]
  (dx/parse r))
;; => {:tag :-document,
;;     :attrs {:encoding nil, :standalone nil, :system-id nil},
;;     :content
;;     ({:tag :a, :attrs {}, :content ({:tag :b, :attrs {}, :content ("c")})})}

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
  [:-document
   [::tei/text {:xmlns "http://www.tei-c.org/ns/1.0"}
    [::tei/p
     [:-comment {:data "CDATA and comments can be emitted"}]
     [:-cdata "<--"]]]]))
;; => "<?xml version=\"1.0\"?><text xmlns=\"http://www.tei-c.org/ns/1.0\"><p><!--CDATA and comments can be emitted--><![CDATA[<--]]></p></text>"

(-> (dx/parse "<root/>") (meta) ::dx/location-info)
;; => {:character-offset 0, :column-number 1, :line-number 1}
```

## Run tests

```shell
clojure -X:test
```

## License

Copyright 2022 Rich Hickey, Gregor Middell and [contributors](AUTHORS).

Licensed under the [Eclipse Public
License](http://www.opensource.org/licenses/eclipse-1.0.php).
