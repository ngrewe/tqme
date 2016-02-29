(ns ^{:doc    "Writing the base phaseant/TQC ontology"
      :author "Niels Grewe"}
  phaseant.writer
  (:require [tawny.owl])
  (:use [phaseant.core :only [tqc]])
  (:gen-class)
  )

(defn -main
  "Entry point for writing the ontology"
  [& args]

  (tawny.owl/save-ontology tqc "phaseant.owl" :owl)
  (tawny.owl/save-ontology tqc "phaseant.omn" :omn)

  )
