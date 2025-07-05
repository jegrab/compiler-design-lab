(ns compiler.middleend.jump
  (:require [compiler.middleend.ir :as ir]
            [compiler.frontend.common.id :as id]))

(defn make-label-name [prefix]
  (id/make-label prefix))

(defn- id-to-sym [fun-id]
  (clojure.string/replace (str "fun_" fun-id) #"[:-]" ""))



(defmethod ir/codegen-instruction ::label [label _]
  [(str (id-to-sym (::name label)) ":")])


(defn goto [label-name]
  {::ir/kind ::goto
   ::name label-name})

(defmethod ir/codegen-instruction ::goto [goto _]
  [(str "jmp" (id-to-sym (::name goto)))])


(defn if-true-jump [test-loc-id target-label]
  {::ir/kind ::jump-if-true
   ::test test-loc-id
   ::target-label target-label})

(defmethod ir/codegen-instruction ::jump-if-true [instr loc-mapper]
  (let [test-loc (loc-mapper (::test instr))
        test-loc-code (ir/read-location test-loc)]
    [(str "cmp $0, " test-loc-code)
     (str "jne " (id-to-sym (::target-label instr)))]))

(defn if-false-jump [test-loc-id target-label]
  {::ir/kind ::jump-if-false
   ::test test-loc-id
   ::target-label target-label})

(defmethod ir/codegen-instruction ::jump-if-false [instr loc-mapper]
  (let [test-loc (loc-mapper (::test instr))
        test-loc-code (ir/read-location test-loc)]
    [(str "cmp $0, " test-loc-code)
     (str "je " (id-to-sym (::target-label instr)))]))

(defn return [ret-value-loc]
  {::ir/kind ::return
   ::a ret-value-loc})

(defmethod ir/codegen-instruction ::return [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        size (::ir/size a-loc)]
    (when-not (= 32 size) (throw (Exception. "returning non 32 bit values not currently supported")))
    [(str "movslq " (ir/read-location a-loc) ", %rdi")
     "leave"
     "ret"
     ""]))