(ns ^{:doc    "Core definitions for temporally qualified material entities."
      :author "Niels Grewe"}
 tqc.core
  (:require [tqc.base :as b]
            [tawny.owl :as o])
  (:use [tawny.owl]))

(defn- ml "Generates a multi line string" [& strings] (clojure.string/join "\n" strings))

(defontology tqc
  :iri "http://www.halbordnung.de/ontologies/tqme.owl"
  :prefix "tqc:"
  :iri-gen b/iri-generate)

(owl-import b/bfo)

(defclass tqme
  :label "temporally qualified material entity"
  :super b/independent_continuant
  :comment (ml "Temporally qualified material entities are material entities"
               "considered only during a portion of their lifetime."
  )
)

(add-superclass b/material_entity tqme)

(defclass phase
  :label "minimal history segment"
  :super b/occurrent
  :comment (ml "History segments are restricted counterparts of histories. They"
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

;; Match the history axiom
(add-superclass phase (owl-only b/part_of_occurrent (owl-not b/process_profile)))

(defoproperty phase-of
  :label "minimal history segment of"
  :super b/specifically_depends_on_at_all_times
  :domain phase
  :range tqme
  :characteristic :functional)

(defoproperty has-phase
  :label "has minimal history segment"
  :inverse phase-of
  :characteristic :inversefunctional)

(as-equivalent phase (owl-and (owl-some b/part_of_occurrent b/history)
                              (owl-some b/exists_at b/zero-dimensional_temporal_region)))

(add-superclass phase (owl-some phase-of o/owl-thing))

(add-superclass b/material_entity (owl-some b/has_history b/history))


(defoproperty has-min-tqc
              :label "has minimal temporally qualified material entity"
              :domain tqme
              :comment (ml "The relation between a material entity (of temporally maximal qualification) and one of its minimal ones."
                            "Since minimally qualified material entites are themselves material entities, they may themselves be in the domain of this relation."
                            )
              )

(add-subchain has-min-tqc [b/has_history b/has_occurrent_part phase-of])

(defoproperty min-tqc-of
              :label "minimal temporally qualified material entity of"
              :range b/material_entity
              :inverse has-min-tqc)

(defoproperty has-max-tqc
              :label "has maximal material entity"
              :range b/material_entity
              :comment "the relation between a temporally qualified material entity and a maximal one"
              )

(add-subchain has-max-tqc [has-phase b/part_of_occurrent b/history_of])

(defoproperty max-tqc-of
              :label "maximal material entity of"
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
  "Returns a restriction on a material entity to be permanently specifically of a type"
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
  {:doc      "Returns a restriction on a material entity that is scoped as permanently specific."
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