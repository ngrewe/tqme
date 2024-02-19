(ns ^{:doc    "Core definitions for temporally qualified material entities."
      :author "Niels Grewe"}
 de.halbordnung.ontologies.tqme.core
  (:require [de.halbordnung.ontologies.tqme.base :as b]
            [tawny.owl :as o])
  (:use [tawny.owl]))

(defn- ml "Generates a multi line string" [& strings] (clojure.string/join "\n" strings))

(defontology tqme-ontology
  :iri "http://www.halbordnung.de/ontologies/tqme.owl"
  :prefix "tqme:"
  :iri-gen b/iri-generate)

(owl-import b/bfo)
(owl-import b/tawny)

(defclass tqme
  :label "temporally qualified material entity"
  :super b/independent_continuant
  :comment (ml "Temporally qualified material entities are material entities"
               "considered only during a portion of their lifetime."
  )
)

(add-superclass tqme-ontology b/material_entity tqme)

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

(defoproperty phase-of
  :label "minimal history segment of"
  :domain phase
  :range tqme
  :characteristic :functional)

(defoproperty has-phase
  :label "has minimal history segment"
  :inverse phase-of
  :characteristic :inversefunctional)

(as-equivalent phase (owl-and (owl-some b/occurrent_part_of b/history)
                              (owl-some b/exists_at b/zero-dimensional_temporal_region)))

(add-superclass tqme-ontology phase (owl-some phase-of o/owl-thing))

(add-superclass tqme-ontology b/material_entity (owl-some b/has_history b/history))


(defoproperty has-min-tqme
              :label "has minimal temporally qualified material entity"
              :domain tqme
              :comment (ml "The relation between a material entity (of temporally maximal qualification) and one of its minimal ones."
                            "Since minimally qualified material entites are themselves material entities, they may themselves be in the domain of this relation."
                            )
              )

(add-subchain tqme-ontology has-min-tqme [b/has_history b/has_occurrent_part phase-of])

(defoproperty min-tqme-of
              :label "minimal temporally qualified material entity of"
              :range b/material_entity
              :inverse has-min-tqme)

(defoproperty has-max-tqme
              :label "has maximal material entity"
              :range b/material_entity
              :comment "the relation between a temporally qualified material entity and a maximal one"
              )

(add-subchain tqme-ontology has-max-tqme [has-phase b/occurrent_part_of b/history_of])

(defoproperty max-tqme-of
              :label "maximal material entity of"
              :domain b/material_entity
              :inverse has-max-tqme)

(add-subproperty tqme-ontology min-tqme-of has-max-tqme)
(add-subproperty tqme-ontology has-min-tqme max-tqme-of)

;; we steal a bit of stuff from tawny.owl to build our generators
(defn
  guess-type-args
  {:doc  "Broadcasting version of guess-type"
   :private true}
  ;; unwind to avoid variadic args for the most common calls.
  ([a]
   (guess-type a))
  ([a b]
   (or
     (guess-type a)
     (guess-type b)))
  ([a b c]
   (or
     (guess-type a)
     (guess-type b)
     (guess-type c)))
  ([a b c d]
   (or
     (guess-type a)
     (guess-type b)
     (guess-type c)
     (guess-type d)))
  ([a b c d & args]
   (or
     (guess-type a)
     (guess-type b)
     (guess-type c)
     (guess-type d)
     ;; guess-type already copes with collections
     (guess-type args))))

(defmulti phase-perm-spec
  "Returns a restriction on an phase that is scoped as permanently specific"
  #'guess-type-args)

(defmulti perm-spec
  "Returns a restriction on a material entity to be permanently specifically of a type"
  #'guess-type-args)


(defmethod phase-perm-spec nil [& rest]
  (apply guess-type-error rest))

(defno ophase-perm-spec
  {:doc      "Returns a restriction on an phase that is scoped as permanently specific."
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some phase-of class))

(defmethod phase-perm-spec :tawny.owl/object [& rest]
  (apply ophase-perm-spec rest))


(defno object-perm-spec
  {:doc      "Returns a restriction on a material entity that is scoped as permanently specific."
   :arglists '([& clazzes] [ontology & clazzes])}
  [o class]
  (owl-some b/has_history (phase-perm-spec o class)))

(defn perm-spec
       [class]
       class)

(defn temp
  [ctor relation class]
  (owl-some
            has-max-tqme
            (owl-some
                      has-min-tqme
                      (ctor
                            relation
                            (owl-some min-tqme-of class)
                            )
                      )
            )
  )

(defn perm-gen
  [ctor relation class]
  (owl-and
    (owl-some has-min-tqme
              (ctor relation (owl-some min-tqme-of class)))
    (owl-only has-min-tqme
              (ctor relation (owl-some min-tqme-of class)))

    )
  )
