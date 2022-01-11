(ns clojure.data.xml.protocols)

;; XML names can be any data type that has at least a namespace uri and a name slot

(defprotocol EventGeneration
  "Protocol for generating new events based on element type"
  (gen-event [item]
    "Function to generate an event for e.")
  (next-events [item next-items]
    "Returns the next set of events that should occur after e.  next-events are the
     events that should be generated after this one is complete."))

(defprotocol AsXmlString
  (xml-str [node] "Serialize atribute value or content node"))
