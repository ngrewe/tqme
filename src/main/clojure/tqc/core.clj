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
  :label "minimal history fragment"
  :super b/occurrent
  :comment (ml "History fragments are restricted counterparts of histories. They"
               "are temporal parts of histories and share the"
               "following characteristics:"
               ""
               "  * They are comprised of a sum of processes."
               "  * The process sum that makes up the part is the"
               "    totality of all processes taking place in a"
               "    certain spatiotemporal region."
               "  * For present purposes, the spatiotemporal region"
               "    they take place in is a 0-dimensional one "
               "    (they only occur at instants)"
               "  * They pertain to a single entity."))

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
  :label "minimal history fragment of"
  :super b/specifically_depends_on_at_all_times
  :domain phase
  :range tq-continuant
  :characteristic :functional)

(defoproperty has-phase
  :label "has minimal history fragment"
  :inverse phase-of
  :characteristic :inversefunctional)

(as-equivalent phase (owl-and (owl-some b/part_of_occurrent b/history)
                              (owl-some b/exists_at b/zero-dimensional_temporal_region)))


(add-superclass phase (owl-some phase-of tq-continuant))

(add-superclass tq-continuant (owl-some has-phase phase))

(add-superclass b/material_entity (owl-some b/has_history b/history))


(defoproperty has-min-tqc
              :label "has minimal temporally qualified continuant"
              :domain b/material_entity
              :comment (ml "The relation between a continuant (of temporally maximal qualification) and one of its minimal ones."
                            "Since minimally qualified continuants are themselves continuants, they may themselves be in the domain of this relation."
                            )
              )

(add-subchain has-min-tqc [b/has_history b/has_occurrent_part phase-of])

(defoproperty min-tqc-of
              :label "minimal temporally qualified continuant of"
              :range b/material_entity
              :inverse has-min-tqc)

(defoproperty has-max-tqc
              :label "has maximal continuant"
              :range b/material_entity
              :comment "the relation between a temporally qualified continuant and a maximal one"
              )

(add-subchain has-max-tqc [has-phase b/part_of_occurrent b/history_of])

(defoproperty max-tqc-of
              :label "maximal continuant of"
              :domain b/material_entity
              :inverse has-max-tqc)

(add-subproperty min-tqc-of has-max-tqc)
(add-subproperty has-min-tqc max-tqc-of)

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

(defmulti perm-spec
  "Returns a restriction on a continuant to be permanently specifically of a type"
  #'guess-type-args)


(defmethod phase-perm-spec nil [& rest]
  (apply guess-type-error rest))

(defmontfn ophase-perm-spec
  {:doc      "Returns a restriction on an phase that is scoped as permanently specific."
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some o phase-of class))

(defmethod phase-perm-spec :tawny.owl/object [& rest]
  (apply ophase-perm-spec rest))


(defmontfn object-perm-spec
  {:doc      "Returns a restriction on a continuant that is scoped as permanently specific."
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some o b/has_history (phase-perm-spec o class)))

(defn perm-spec
       [o class]
       class)

(defn temp
  [o ctor relation class]
  (owl-some o
            has-max-tqc
            (owl-some o
                      has-min-tqc
                      (ctor o
                            relation
                            (owl-some o min-tqc-of class)
                            )
                      )
            )
  )

(defn perm-gen
  [o ctor relation class]
  (owl-and
    (owl-some o has-min-tqc
              (ctor o relation (owl-some o min-tqc-of class)))
    (owl-only o has-min-tqc
              (ctor o relation (owl-some o min-tqc-of class)))

    )
  )