(ns compiler.middleend.arithmetic
  (:require [compiler.middleend.ir :as ir]))

(defn bin-op [kind a b target]
  {::ir/kind kind
   ::a a
   ::b b
   ::target target})

(defn not-both-memory [a b]
  (not (and (ir/memory? a) (ir/memory? b))))

(defmethod ir/codegen ::add [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        helper-code (ir/read-location (loc-mapper ::ir/helper))
        suffix (ir/size-suffix a-loc)]
    (cond 
      (and (= a-loc t-loc) (not-both-memory a-loc b-loc)) 
      [(str "add" suffix " " (ir/read-location b-loc) ", " (ir/read-location a-loc))]
      (and (= b-loc t-loc) (not-both-memory a-loc b-loc))
      [(str "add" suffix " " (ir/read-location a-loc) ", " (ir/read-location b-loc))]
      :default
      [(str "mov" suffix " " (ir/read-location b-loc) ", " helper-code)
       (str "add" suffix " " (ir/read-location a-loc) ", " helper-code)
       (str "mov" suffix " " helper-code ", "(ir/read-location t-loc))])))

(defmethod ir/codegen ::sub [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        helper-code (ir/read-location (loc-mapper ::ir/helper))
        suffix (ir/size-suffix a-loc)]
    (cond
      (and (= a-loc t-loc) (not-both-memory a-loc b-loc))
      [(str "add" suffix " " (ir/read-location b-loc) ", " (ir/read-location a-loc))]
      (and (= b-loc t-loc) (not-both-memory a-loc b-loc))
      [(str "add" suffix " " (ir/read-location a-loc) ", " (ir/read-location b-loc))]
      :default
      [(str "mov" suffix " " (ir/read-location b-loc) ", " helper-code)
       (str "add" suffix " " (ir/read-location a-loc) ", " helper-code)
       (str "mov" suffix " " helper-code ", " (ir/read-location t-loc))])))






