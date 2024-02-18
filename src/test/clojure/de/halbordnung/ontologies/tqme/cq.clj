(ns de.halbordnung.ontologies.tqme.cq
  (:require [tawny.owl :as o]
            [tawny.reasoner :as r]
            [de.halbordnung.ontologies.tqme.core :as c]
            [de.halbordnung.ontologies.tqme.base :as b]
            [clojure.test]
            )
  [:use clojure.test]
  (:import (org.semanticweb.owlapi.reasoner InconsistentOntologyException OWLReasoner)))

(def to nil)
(defn createtestontology []
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology
      :iri "http://iri/"
      :prefix "iri:"))))

(defn tqme-fixture [test]
  (binding
   [r/*reasoner-progress-monitor*
    (atom r/reasoner-progress-monitor-silent)]
    (tawny.reasoner/reasoner-factory :hermit)
    (createtestontology)
    (o/owl-import to c/tqme-ontology)
    (test)))

(defmethod clojure.test/report :end-test-var [m]
  (o/save-ontology to (clojure.core/str (-> m :var meta :name) ".owl") :owl))

(defn write-tqme [test]
  (o/save-ontology c/tqme-ontology "tqme.owl" :owl)
  (test))

(defn satisfiable? [ontology expr]
  (.flush ^OWLReasoner (r/reasoner ontology))
  (try
    (.isSatisfiable ^OWLReasoner (r/reasoner ontology) expr)
    (catch InconsistentOntologyException e false)
    )
  )

(use-fixtures :each tqme-fixture)
(use-fixtures :once write-tqme)

(deftest tgr_cm+
  (def tooth (o/owl-class to "tooth"
               :super b/material_entity))
  (def organism (o/owl-class to "organism"
               :super b/material_entity))
  (def human (o/owl-class to "human"
               :super organism))
  (o/disjoint-classes to (list tooth organism))
  (o/add-superclass to tooth
                    (c/temp to o/owl-some b/continuant_part_of_at_all_times organism))


  (o/add-superclass to human
                    (c/temp to o/owl-only b/has_continuant_part_at_all_times (o/owl-not tooth)))
  (is (r/consistent? to) "Ontology is consistent")
  (is (satisfiable? to (o/owl-and tooth
                                  (o/owl-some
                                    c/has-min-tqme
                                    (o/! (o/owl-some
                                                        b/continuant_part_of_at_all_times
                                                        (o/owl-some c/min-tqme-of organism)))))
                    )
      "A tooth for which there is a time it is not part of any organism → OK")
  (is (satisfiable? to (o/owl-and human
                                  (o/owl-some c/has-min-tqme
                                              (o/owl-some b/has_continuant_part_at_all_times
                                                          (o/owl-some
                                                                      c/min-tqme-of
                                                                      tooth)))))
      "A human having teeth at some time → OK"))

(deftest tgr_cm-
  (def color (o/owl-class to "color"
               :super b/quality))
  (def red-color (o/owl-class to "red-color"
               :super color))
  (def green-color (o/owl-class to "green-color"
               :super color))
  (o/disjoint-classes to (list green-color red-color))
  (def apple (o/owl-class to "apple"
               :super (o/owl-and b/material_entity
                                 (o/owl-some c/has-min-tqme (o/owl-some b/specifically_depended_on_by green-color)))))
  (is (satisfiable? to (o/owl-and apple
                                  (o/owl-some c/has-min-tqme (o/owl-only b/specifically_depended_on_by
                                                                                     (o/|| red-color (o/! color))))))
      "An apple that at some time has no other color than red → OK"))

(deftest tgr_cp1
  (def birth (o/owl-class to "birth"
               :super b/process))
  (def mammal (o/owl-class to "mammal"
                    :super (o/owl-and b/material_entity
                                      (o/owl-some c/has-max-tqme
                                                  (o/owl-some c/has-min-tqme
                                                              (o/owl-some b/participates_in_at_all_times birth)))
                                      )
                    )
    )
  (.flush ^OWLReasoner (r/reasoner to))
  (is (satisfiable? to (o/owl-and mammal
                                  (o/owl-some c/has-max-tqme
                                              (o/owl-some c/has-min-tqme
                                                          (o/! (o/owl-some
                                                                              b/participates_in_at_all_times
                                                                              birth))))))
      "A mammal that at some time does not participate in a birth process → OK"))

(deftest psr_cm+
  (def brain (o/owl-class to "brain"
               :super b/material_entity)) (let
                                              [has-brain (o/object-property to "has-brain"
                                                                            :super b/has_continuant_part_at_all_times
                                                                            :range brain
                                                                            :characteristic :functional)

                                               part-of-brain (o/object-property to "part-of-brain"
                                                                                :super b/continuant_part_of_at_all_times
                                                                                :range brain
                                                                                :characteristic :inversefunctional)
                                               ventricle (o/owl-class to "ventricle"
                                                                      :super (o/owl-and b/material_entity
                                                                                        (c/perm-spec to (o/owl-some part-of-brain brain))))
                                               human (o/owl-class to "human"
                                                                  :super (o/owl-and b/material_entity
                                                                                    (c/perm-spec to (o/owl-some has-brain brain))))

                                               joes-brain (o/individual to "joes-brain"
                                                                        :type brain)

                                               other-brain (o/individual to "other-brain"
                                                                         :type brain
                                                                         :different joes-brain)

                                               joe (o/individual to "joe"
                                                                 :type human
                                                                 :fact (o/fact has-brain joes-brain))

                                              p1 (o/individual to "phase1"
                                                                :type c/phase
                                                                :fact (o/fact c/phase-of joe))
                                               mrx (o/individual to "mrx"
                                                                 :fact (o/fact c/has-phase p1)
                                                                 :comment "Mr X should in fact be identical with Joe")

                                            ]
                                           (is (r/consistent? to)
                                               "Consistent before bad axiom")

                                           (o/with-probe-axioms to
                                             (o/add-fact to mrx (o/fact has-brain other-brain))

                                             (is (not (r/consistent? to))
                                                 "There is a time in which Joe does not have his original brain → owl:Nothing"))

      ;; Sanity check: Were the probe axioms really removed?
                                           (is (r/consistent? to))
                                           (is (not (satisfiable? to (o/owl-and ventricle
                                                                                (o/!
                                                                                     (o/owl-some  part-of-brain brain))
                                                                                (o/owl-some  part-of-brain brain))))
                                               "A brain ventricle that is part of a human brain at some time and that is not part of any human brain at another time → owl:Nothing")

      ;; We have a third example here but that's essentially negation of the entire scope, so we don't do it.
))

(deftest psr_cm-

  (o/owl-class to "sex"
               :super b/quality)
  (o/owl-class to "male-sex"
               :super "sex")
  (o/owl-class to "female-sex"
               :super "sex")
  (o/disjoint-classes to (list "male-sex" "female-sex"))
  (o/owl-class to "mammal"
               :super (o/owl-and to b/material_entity
                                 (c/perm-spec to (o/exactly to 1 b/specifically_depended_on_by "sex"))))

  (is (not (satisfiable? to (o/owl-and to "mammal"
                                       (o/owl-some to b/specifically_depended_on_by "male-sex")
                                       (o/owl-some to b/specifically_depended_on_by "female-sex"))))
      "A mammal that is biologically male at some time and biologically female at some other time → owl:Nothing"))

(deftest psr_cp1
  (o/owl-class to "life"
               :super b/history)
  (o/owl-class to "organism"
               :super (o/owl-and to b/material_entity
                                 (c/perm-spec to (o/exactly to 1 b/participates_in_at_all_times "life"))))

  (let
   [joes-life (o/individual to "joes-life"
                            :type "life")
    joe (o/individual to "joe"
                      :type "organism"
                      :fact (o/fact to b/participates_in_at_all_times joes-life))
    other-life (o/individual to "other-life"
                             :type "life"
                             :different joes-life)]
    (o/with-probe-axioms to
      [a (o/add-fact to joe (o/fact to b/participates_in_at_all_times other-life))]

      (is (not (r/consistent? to))
          "Joe participates in two different lives → owl:Nothing"))))

;; PSR_cp2 needs to be reformulated -- it is nonesense as is

(deftest pgr_cm+
  (o/owl-class to "oxygen-molecule"
               :super b/material_entity)
  (o/owl-class to "red-blood-cell"
               :super b/material_entity)
  (o/owl-class to "root-phase"
               :super (o/owl-and to
                                 c/phase
                                 (o/! to (o/owl-some to b/has_proper_occurrent_part o/owl-thing))
                                 (o/! to (o/owl-some to c/phase-of (o/owl-some to b/has_continuant_part_at_all_times
                                                                               "oxygen-molecule")))))
  (o/disjoint-classes to (list "oxygen-molecule" "red-blood-cell"))

  (o/add-superclass to "red-blood-cell"
                    (c/perm-gen to o/owl-some b/has_continuant_part_at_all_times "oxygen-molecule"))

    (o/with-probe-axioms to
                         [a (o/add-subclass to
                              (o/owl-some to c/min-tqme-of "red-blood-cell")
                              (o/! to (o/owl-some to b/has_continuant_part_at_all_times
                                                  (o/owl-some to c/min-tqme-of "oxygen-molecule")))
                              )]
                         (is (not (r/consistent? to))
                             "There are red blood cells without oxygen molecules → owl:Nothing"))
)


(deftest pgr_cm-

  (o/owl-class to "apple"
               :super b/material_entity)
  (o/owl-class to "colour"
               :super b/quality)

  (o/add-superclass to "apple"
                    (c/perm-gen to o/owl-some b/specifically_depended_on_by "colour"))

  (.flush ^OWLReasoner (r/reasoner to))
    (is (r/consistent? to)
        "Consistent TQME ontology for apple/colour")

    (o/with-probe-axioms to
      [a (o/add-subclass to
                         (o/owl-some to c/min-tqme-of "apple")
                         (o/! to (o/owl-some to b/specifically_depended_on_by
                                             (o/owl-some to c/min-tqme-of "colour"))))]
      (is (not (r/consistent? to))
          "There are apples without a colour → owl:Nothing"))
  )

(deftest pgr_cp1

  (o/owl-class to "organism"
               :super b/material_entity)
  (o/owl-class to "eco-process"
               :super b/process)

  (o/add-superclass to "organism"
                    (o/owl-and to
                               (o/owl-some to c/has-min-tqme (o/owl-some to b/participates_in_at_all_times "eco-process"))
                               (o/owl-only to c/has-min-tqme (o/owl-some to b/participates_in_at_all_times "eco-process"))
                               )
                    )

  (is (r/consistent? to)
      "Consistent TQME ontology for participation")

  (o/with-probe-axioms to
      [a (o/add-subclass to
                         (o/owl-some to c/min-tqme-of "organism")
                         (o/! to (o/owl-some to b/participates_in_at_all_times "eco-process")))]
      (is (not (r/consistent? to))
          "There are organisms that do not participate in any ecologic process at certain times → owl:Nothing"))
  )


(deftest pgr_transitivity

  (o/owl-class to "blood-volume"
               :super b/material_entity)
  (o/owl-class to "red-blood-cell"
               :super b/material_entity)
  (o/owl-class to "oxygen-molecule"
               :super b/material_entity)

  (o/disjoint-classes to (list "blood-volume" "red-blood-cell" "oxygen-molecule"))

  (o/add-superclass to "red-blood-cell"
                    (c/perm-gen to o/owl-some b/has_continuant_part_at_all_times
                                                                  "oxygen-molecule"))

  (o/add-superclass to "blood-volume"
                    (c/perm-gen to o/owl-some b/has_continuant_part_at_all_times
                                                                      "red-blood-cell"))
  (is (r/consistent? to)
      "Consistent TQME ontology for transitive permanent generic parthood")
  (is (not (satisfiable? to
                         (o/owl-and to
                                    (o/owl-some to c/min-tqme-of "blood-volume")
                                    (o/! to (o/owl-some to
                                                        b/has_continuant_part_at_all_times
                                                        (o/owl-some to c/min-tqme-of "oxygen-molecule"))))
                         ))
                           "There times where a blood volume has no oxygen molecules → owl:Nothing")
  )
