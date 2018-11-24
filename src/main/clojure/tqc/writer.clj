(ns ^{:doc    "Writing the base TQC ontology"
      :author "Niels Grewe"}
 tqc.writer
  (:require [tawny.owl])
  (:use [tqc.core :only [tqc]])
  (:gen-class))

(defn -main
  "Entry point for writing the ontology"
  [& args]

  (tawny.owl/save-ontology tqc "tqme.owl" :owl)
  (tawny.owl/save-ontology tqc "tqme.omn" :omn))
