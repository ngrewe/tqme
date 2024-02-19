(ns ^{:doc    "BFO import layer and general declarations for TQMEs."
      :author "Niels Grewe"}
 de.halbordnung.ontologies.tqme.base
  (:require [clj-uuid :as uuid]
            [tawny.read :as r]
            [tawny.memorise]
            [tawny.owl :as o])
  (:use tawny.owl)
  (:import (org.semanticweb.owlapi.model IRI OWLOntology)))

(defonce ^String bfo-iri  "http://purl.obolibrary.org/obo/bfo.owl")

(defonce ^String tawny-iri  "http://www.purl.org/ontolink/tawny")


(defn bfo-resource-iri "Get a reference to the local BFO copy"  ^IRI []
  (IRI/create (clojure.java.io/resource "bfo.owl")))

(defn tawny-resource-iri "Get a reference to the local tawny.owl copy"  ^IRI []
  (IRI/create (clojure.java.io/resource "tawny.owl")))

(defn -scoped "Generate a v5 UUID in the tqme namespace" [^String n]
  ; The namespace here must be kept as is in order to keep IRIs stable
  (uuid/v5 (uuid/v5 uuid/+namespace-url+ "http://www.halbordnung.de/ontologies/tqc.owl")
           n))
(o/defno iri-generate
  "Generate IRIs for the determinist names"
  [o name]
  (iri (str (.get (.getOntologyIRI (.getOntologyID ^OWLOntology o))) "#" (-scoped name))))

(r/defread bfo
  :location (bfo-resource-iri)
  :prefix "bfo"
  :iri bfo-iri
  :viri "http://purl.obolibrary.org/obo/bfo/2020/bfo.owl"
  :filter
  (clojure.core/partial tawny.read/iri-starts-with-filter "http://purl.obolibrary.org/obo/BFO")
  :transform
  (clojure.core/comp r/stop-characters-transform
                     r/exception-nil-label-transform))
(tawny.memorise/remember bfo (clojure.java.io/resource "bfo_memo.clj"))

(r/defread tawny
  :location (tawny-resource-iri)
  :prefix "tawny"
  :iri  tawny-iri
)