(ns compiler.middleend.jump
  (:require [compiler.middleend.ir :as ir]
            [compiler.frontend.common.id :as id]))

(defn make-label-name [prefix]
  (id/make-label prefix))

(defn- id-to-sym [fun-id]
  (clojure.string/replace (str "fun_" fun-id) #"[:-]" ""))

(defn label [label-name]
  {::ir/kind ::label
   ::name label-name})

(defmethod ir/codegen ::label [label _]
  [(str (id-to-sym (::name label)) ":")])


(defn goto [label-name]
  {::ir/kind ::goto
   ::name label-name})

(defmethod ir/codegen ::goto [goto _]
  [(str "jmp" (id-to-sym (::name goto)))])


(defn if-true-jump [test-loc-id target-label]
  {::ir/kind ::jump-if-true
   ::test test-loc-id
   ::target-label target-label})

(defmethod ir/codegen ::jump-if-true [instr loc-mapper]
  (let [test-loc (loc-mapper (::test instr))
        test-loc-code (ir/read-location test-loc)]
    [(str "cmp $0, " test-loc-code)
     (str "jne " (id-to-sym (::target-label instr)))]))

(defn if-false-jump [test-loc-id target-label]
  {::ir/kind ::jump-if-false
   ::test test-loc-id
   ::target-label target-label})

(defmethod ir/codegen ::jump-if-false [instr loc-mapper]
  (let [test-loc (loc-mapper (::test instr))
        test-loc-code (ir/read-location test-loc)]
    [(str "cmp $0, " test-loc-code)
     (str "je " (id-to-sym (::target-label instr)))]))