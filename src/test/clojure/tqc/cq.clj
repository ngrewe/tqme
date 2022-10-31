(ns tqc.cq
  (:require [tawny.owl :as o]
            [tawny.reasoner :as r]
            [tqc.core :as c]
            [tqc.base :as b])
  [:use clojure.test]
  (:import (org.semanticweb.owlapi.reasoner OWLReasoner)))

(def to nil)
(defn createtestontology []
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology
      :iri "http://iri/"
      :prefix "iri:"))))

(defn tqcfixture [test]
  (binding
   [r/*reasoner-progress-monitor*
    (atom r/reasoner-progress-monitor-silent)]
    (tawny.reasoner/reasoner-factory :hermit)
    (createtestontology)
    (o/owl-import to c/tqc)
    (test)))

(defmethod report :end-test-var [m]
  (o/save-ontology to (clojure.core/str (-> m :var meta :name) ".owl") :owl))

(defn writetqc [test]
  (o/save-ontology c/tqc "tqme.owl" :owl)
  (test))

(defn satisfiable? [ontology expr]
  (.flush ^OWLReasoner (r/reasoner ontology))
  (.isSatisfiable ^OWLReasoner (r/reasoner ontology) expr))

(use-fixtures :each tqcfixture)
(use-fixtures :once writetqc)

(deftest tgr_cm+
  (o/owl-class to "tooth"
               :super b/material_entity)
  (o/owl-class to "organism"
               :super b/material_entity)
  (o/owl-class to "human"
               :super "organism")
  (o/disjoint-classes to (list "tooth" "organism"))
  (o/add-superclass to "tooth"
                    (c/temp to o/owl-some b/part_of_continuant_at_all_times "organism"))


  (o/add-superclass to "human"
                    (c/temp to o/owl-only b/has_continuant_part_at_all_times (o/owl-not to "tooth")))
  (is (r/consistent? to) "Ontology is consistent")
  (is (satisfiable? to (o/owl-and to "tooth"
                                  (o/owl-some
                                    to
                                    c/has-min-tqc
                                    (o/! to (o/owl-some to
                                                        b/part_of_continuant_at_all_times
                                                        (o/owl-some to c/min-tqc-of "organism")))))
                    )
      "A tooth for which there is a time it is not part of any organism → OK")
  (is (satisfiable? to (o/owl-and to "human"
                                  (o/owl-some to c/has-min-tqc
                                              (o/owl-some to b/has_continuant_part_at_all_times
                                                          (o/owl-some to
                                                                      c/min-tqc-of
                                                                      "tooth")))))
      "A human having teeth at some time → OK"))

(deftest tgr_cm-
  (o/owl-class to "color"
               :super b/quality)
  (o/owl-class to "red-color"
               :super "color")
  (o/owl-class to "green-color"
               :super "color")
  (o/disjoint-classes to (list "green-color" "red-color"))
  (o/owl-class to "apple"
               :super (o/owl-and to b/material_entity
                                 (o/owl-some to c/has-min-tqc (o/owl-some to b/has_quality_at_all_times "green-color"))))

  (is (satisfiable? to (o/owl-and to "apple"
                                  (o/owl-some to c/has-min-tqc (o/owl-only to b/has_quality_at_all_times
                                                                                     (o/|| to "red-color" (o/! to "color"))))))
      "An apple that at some time has no other color than red → OK"))

(deftest tgr_cp1
  (o/owl-class to "birth"
               :super b/process)
  (o/owl-class to "mammal"
               :super (o/owl-and to b/material_entity
                                 (o/owl-some to c/has-max-tqc
                                             (o/owl-some to c/has-min-tqc
                                                         (o/owl-some to b/participates_in_at_all_times "birth")))
                                 )
               )
  (.flush ^OWLReasoner (r/reasoner to))
  (is (satisfiable? to (o/owl-and to "mammal"
                                  (o/owl-some to c/has-phase
                                              (o/owl-some to c/phase-of
                                                          (o/! to (o/owl-some to
                                                                              b/participates_in_at_all_times
                                                                              "birth"))))))
      "A mammal that at some time does not participate in a birth process → OK"))

(deftest psr_cm+
  (o/owl-class to "brain"
               :super b/material_entity) (let
                                          [has-brain (o/object-property to "has-brain"
                                                                        :super b/has_continuant_part_at_all_times
                                                                        :range "brain"
                                                                        :characteristic :functional)

                                           part-of-brain (o/object-property to "part-of-brain"
                                                                            :super b/part_of_continuant_at_all_times
                                                                            :domain "brain"
                                                                            :characteristic :inversefunctional)
                                           ventricle (o/owl-class to "ventricle"
                                                                  :super (o/owl-and to b/material_entity
                                                                                    (c/perm-spec to (o/owl-some to part-of-brain "brain")))) human (o/owl-class to "human"
                                                                                                                                                                :super (o/owl-and to b/material_entity
                                                                                                                                                                                  (c/perm-spec to (o/owl-some to has-brain "brain"))))

                                           joes-brain (o/individual to "joes-brain"
                                                                    :type "brain")

                                           other-brain (o/individual to "other-brain"
                                                                     :type "brain"
                                                                     :different joes-brain)

                                           joe (o/individual to "joe"
                                                             :type human
                                                             :fact (o/fact to has-brain joes-brain))

                                           p1 (o/individual to "phase1"
                                                            :type c/phase
                                                            :fact (o/fact to c/phase-of joe))
                                           mrx (o/individual to "mrx"
                                                             :fact (o/fact to c/has-phase p1)
                                                             :comment "Mr X should in fact be identical with Joe")

                                           error-fact (o/fact to has-brain other-brain)]
                                           (is (r/consistent? to)
                                               "Consistent before bad axiom")

                                           (o/with-probe-axioms to
                                             [a (o/add-fact to mrx error-fact)]

                                             (is (not (r/consistent? to))
                                                 "There is a time in which Joe does not have his original brain → owl:Nothing"))

      ;; Sanity check: Were the probe axioms really removed?
                                           (is (r/consistent? to))
                                           (is (not (satisfiable? to (o/owl-and to ventricle
                                                                                (o/! to
                                                                                     (o/owl-some to part-of-brain "brain"))
                                                                                (o/owl-some to part-of-brain "brain"))))
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
                                 (c/perm-spec to (o/exactly to 1 b/has_quality_at_all_times "sex"))))

  (is (not (satisfiable? to (o/owl-and to "mammal"
                                       (o/owl-some to b/has_quality_at_all_times "male-sex")
                                       (o/owl-some to b/has_quality_at_all_times "female-sex"))))
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
                              (o/owl-some to c/min-tqc-of "red-blood-cell")
                              (o/! to (o/owl-some to b/has_continuant_part_at_all_times
                                                  (o/owl-some to c/min-tqc-of "oxygen-molecule")))
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
                    (c/perm-gen to o/owl-some b/has_quality_at_all_times "colour"))

  (.flush ^OWLReasoner (r/reasoner to))
    (is (r/consistent? to)
        "Consistent TQC ontology for apple/colour")

    (o/with-probe-axioms to
      [a (o/add-subclass to
                         (o/owl-some to c/min-tqc-of "apple")
                         (o/! to (o/owl-some to b/has_quality_at_all_times
                                             (o/owl-some to c/min-tqc-of "colour"))))]
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
                               (o/owl-some to c/has-min-tqc (o/owl-some to b/participates_in_at_all_times "eco-process"))
                               (o/owl-only to c/has-min-tqc (o/owl-some to b/participates_in_at_all_times "eco-process"))
                               )
                    )

  (is (r/consistent? to)
      "Consistent TQC ontology for participation")

  (o/with-probe-axioms to
      [a (o/add-subclass to
                         (o/owl-some to c/min-tqc-of "organism")
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
      "Consistent TQC ontology for transitive permanent generic parthood")
  (is (not (satisfiable? to
                         (o/owl-and to
                                    (o/owl-some to c/min-tqc-of "blood-volume")
                                    (o/! to (o/owl-some to
                                                        b/has_continuant_part_at_all_times
                                                        (o/owl-some to c/min-tqc-of "oxygen-molecule"))))
                         ))
                           "There times where a blood volume has no oxygen molecules → owl:Nothing")
  )
