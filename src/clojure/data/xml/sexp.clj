(ns clojure.data.xml.sexp)

(defprotocol AsElements
  (as-elements [expr] "Return a seq of elements represented by an expression."))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (as-elements [v]
    (let [[tag & [attrs & after-attrs :as content]] v
          [attrs content]                           (if (map? attrs)
                                                      [(into {} (for [[k v] attrs]
                                        [k (str v)]))
                             after-attrs]
                                                      [{} content])]
      [{:tag tag :attrs attrs :content (mapcat as-elements content)}]))

  clojure.lang.ISeq
  (as-elements [s]
    (mapcat as-elements s))

  clojure.lang.Keyword
  (as-elements [k]
    [{:tag k :attrs {} :content (list)}])

  java.lang.String
  (as-elements [s]
    [s])

  nil
  (as-elements [_] nil)

  java.lang.Object
  (as-elements [o]
    [(str o)]))



