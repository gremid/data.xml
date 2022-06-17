(ns gremid.data.xml.rnc
  (:import
   (com.thaiopensource.relaxng.translate Driver)
   (java.io File)))

(defn ->rng
  [^File rnc ^File rng]
  (when-not (= 0 (.run (Driver.) (into-array String (map str [rnc rng]))))
    (throw (ex-info "Error while converting RNC to RNG" {:rnc rnc :rng rng}))))

