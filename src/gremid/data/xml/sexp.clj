(ns gremid.data.xml.sexp)

(defprotocol AsElements
  (as-elements [expr] "Return a seq of elements represented by an expression."))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (as-elements [v]
    (let [[tag & content]       v
          [attrs & after-attrs] content
          [attrs content]       (if (map? attrs)
                                  [attrs after-attrs]
                                  [{} content])]
      [{:tag     tag
        :attrs   attrs
        :content (mapcat as-elements content)}]))

  clojure.lang.ISeq
  (as-elements [s] (mapcat as-elements s))

  clojure.lang.Keyword
  (as-elements [k]
    [{:tag     k
      :attrs   {}
      :content (list)}])

  java.lang.String
  (as-elements [s] (list s))

  nil
  (as-elements [_] nil)

  java.lang.Object
  (as-elements [o] (list (str o))))



