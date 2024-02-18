(ns ^{:doc    "Writing the base TQME ontology"
      :author "Niels Grewe"}
de.halbordnung.ontologies.tqme.writer
  (:require [tawny.owl])
  (:use [de.halbordnung.ontologies.tqme.core :only [tqme-ontology]])
  (:gen-class))

(defn -main
  "Entry point for writing the ontology"
  [& args]

  (tawny.owl/save-ontology tqme-ontology "tqme.owl" :owl)
  (tawny.owl/save-ontology tqme-ontology "tqme.omn" :omn))
