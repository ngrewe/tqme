(ns ^{:doc    "Core definitions for continuant profiles/temporally qualified continuants."
      :author "Niels Grewe"}
 tqc.core
  (:require [tqc.base :as b])
  (:use [tawny.owl]))

(defn- ml "Generates a multi line string" [& strings] (clojure.string/join "\n" strings))

(defontology tqc
  :iri "http://www.halbordnung.de/ontologies/tqc.owl"
  :prefix "tqc:"
  :iri-gen b/iri-generate)

(owl-import b/bfo)

;; holder for the new entities
(defclass xntity
  :label "*entity"
  :comment "holder type for tqc entities")
(defclass phase
  :label "phase"
  :comment (ml "Phases are restricted counterparts of histories. They"
               "are temporal parts of histories and share the"
               "following characteristics:"
               ""
               "  * They are comprised of a sum of processes."
               "  * The process sum that makes up the part is the"
               "    totality of all processes taking place in a"
               "    certain spatiotemporal region."
               "  * They pertain to a single entity."))

(as-equivalent phase (owl-some b/part_of_occurrent b/history))

(defclass tq-continuant
          :label "temporally qualified continuant"
          :comment (ml "A temporally qualified continuant is an analog of a"
               "process profile in the domain of continuants. Just as"
               "a process having a certain profile implies its being"
               "such and such in a specific structural dimension,"
               "being a temporally qualified continuant implies that a"
               "continuant shares a certain feature over a specific"
               "period of its existance."
               ""
               "Each temporally qualified continuants maps to a"
               "specific subinterval of the history of the"
               "continuant."))

(as-subclasses
 xntity
 :disjoint :cover
 phase
 tq-continuant)

;; Match the history axiom
(add-superclass phase (owl-only b/part_of_occurrent (owl-not b/process_profile)))

(defoproperty phase-of
  :label "phase of"
  :super b/specifically_depends_on_at_all_times
  :domain phase
  :range tq-continuant
  :characteristic :functional)

(defoproperty has-phase
  :label "has phase"
  :inverse phase-of
  :characteristic :inversefunctional)
(add-subproperty phase-of b/history_of)

(add-superclass phase (owl-some phase-of tq-continuant))

(add-superclass tq-continuant (owl-some has-phase phase))

(add-superclass b/material_entity (owl-some b/has_history b/history))

;; we steal a bit of stuff from tawny.owl to build our generators
(defmontfn
  guess-type-args
  {:doc     "Broadcasting version of guess-type"
   :private true}
  [o & args]
  (guess-type o args))

(defmulti phase-perm-spec
  "Returns a restriction on an phase that is scoped as permanently specific"
  #'guess-type-args)

(defmulti phase-temp
  "Returns a restriction on an phase that is scoped as temporary"
  #'guess-type-args)

(defmulti perm-spec
  "Returns a restriction on a continuant to be permanently specifically of a type"
  #'guess-type-args)

(defmulti temp
  "Returns a restriction on a continuant to be temporarily of a type"
  #'guess-type-args)

(defmethod phase-perm-spec nil [& rest]
  (apply guess-type-error rest))

(defmethod phase-temp nil [& rest]
  (apply guess-type-error rest))

(defmethod perm-spec nil [& rest]
  (apply guess-type-error rest))

(defmethod temp nil [& rest]
  (apply guess-type-error rest))

(defmontfn ophase-perm-spec
  {:doc      "Returns a restriction on an phase that is scoped as permanently specific."
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some o phase-of class))

(defmethod phase-perm-spec :tawny.owl/object [& rest]
  (apply ophase-perm-spec rest))

(defmontfn ophase-temp
  {:doc      "Returns a restriction on an phase that is scoped as temporary"
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-and o
           (owl-some o b/has_occurrent_part (phase-perm-spec o class))
           (owl-some o b/has_proper_occurrent_part
                     (owl-and o phase
                              (owl-not (owl-some b/has_proper_occurrent_part owl-thing))
                              (phase-perm-spec o class)))))

(defmethod phase-temp :tawny.owl/object [& rest]
  (apply ophase-temp rest))

(defmontfn object-perm-spec
  {:doc      "Returns a restriction on a continuant that is scoped as permanently specific."
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some o b/has_history (phase-perm-spec o class)))

(defmethod perm-spec :tawny.owl/object [& rest]
  (apply object-perm-spec rest))

(defmontfn object-temp
  {:doc      "Returns a restriction on an phase that is scoped as temporary"
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some o b/has_history (phase-temp o class)))

(defmethod temp :tawny.owl/object [& rest]
  (apply object-temp rest))

(defmontfn
  add-phase-perm-gen
  {:doc      "Adds a phase class representing permanently generic association."
   :arglists '([name & clazz] [ontology name & clazz])}
  [o name clazz]
  (add-subclass o phase name)
  (add-equivalent o name
                  (owl-and o (owl-or o (phase-perm-spec o clazz)
                                     (owl-some o b/has_proper_occurrent_part name))
                           (owl-only o b/has_proper_occurrent_part name))))

(defmontfn
  perm-gen
  {:doc      (ml "Return an axiom representing permanent generic association"
                 "with the type. Implicitly creates a phase class with the"
                 "specified name.")
   :arglists '([name & clazz] [ontology name & clazz])}
  [o name clazz]
  (add-phase-perm-gen o name clazz)
  (owl-some o b/has_history name))

