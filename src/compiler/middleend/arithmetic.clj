(ns compiler.middleend.arithmetic
  (:require [compiler.middleend.ir :as ir]))

(defn bin-op [kind a b target]
  {::ir/kind kind
   ::a a
   ::b b
   ::target target})

(defn un-op [kind a target]
  {::ir/kind kind
   ::a a
   ::target target})



(defn- make-code [asm-instr & operand-locs]
  (let [suffix (ir/size-suffix (first operand-locs))]
    (apply str asm-instr suffix " " (clojure.string/join ", " (map ir/read-location operand-locs)))))

(defn- codegen-commutative-binop [name instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::ir/helper)]
    (cond
      (and (= a-loc t-loc) (ir/not-both-memory a-loc b-loc))
      [(make-code name b-loc a-loc)]
      (and (= b-loc t-loc) (ir/not-both-memory a-loc b-loc))
      [(make-code name a-loc b-loc)]
      :else
      [(make-code "mov" b-loc h-loc)
       (make-code name a-loc h-loc)
       (make-code "mov" h-loc t-loc)])))

(defn- codegen-unop [name instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        t-loc (loc-mapper (::target instr))]
    (if (= a-loc t-loc)
      [(make-code name t-loc)]
      [(make-code "mov" a-loc t-loc)
       (make-code name t-loc)])))

(defmethod ir/codegen ::negate [instr loc-mapper]
  (codegen-unop "neg" instr loc-mapper))

(defmethod ir/codegen ::add [instr loc-mapper]
  (codegen-commutative-binop "add" instr loc-mapper))

(defmethod ir/codegen ::sub [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::ir/helper)]
    (cond
      (and (= a-loc t-loc) (ir/not-both-memory a-loc b-loc))
      [(make-code "sub" b-loc a-loc)]
      :else
      [(make-code "mov" a-loc h-loc)
       (make-code "sub" b-loc h-loc)
       (make-code "mov" h-loc t-loc)])))

(defmethod ir/codegen ::mul [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))]
    [(make-code "imul" a-loc b-loc t-loc)]))


(defmethod ir/special-register? ::ir/accumulator [_] true)
(defmethod ir/special-register? ::ir/data [_] true)

(defmethod ir/codegen ::div [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        size (::ir/size a-loc)]
    (case size
      16 (throw (Exception. "16 bit division not implemented"))
      32 [(make-code "mov" a-loc (ir/loc-of-reg ::ir/accumulator size))
          "cdq"
          (make-code "idiv" b-loc)
          (make-code "mov" (ir/loc-of-reg ::ir/accumulator size) t-loc)]
      64 (throw (Exception. "64 bit division not implemented")))))

(defmethod ir/codegen ::mod [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        size (::ir/size a-loc)]
    (case size
      16 (throw (Exception. "16 bit division not implemented"))
      32 [(make-code "mov" a-loc (ir/loc-of-reg ::ir/accumulator size))
          "cdq"
          (make-code "idiv" b-loc)
          (make-code "mov" (ir/loc-of-reg ::ir/data size) t-loc)]
      64 (throw (Exception. "64 bit division not implemented")))))


(defmethod ir/codegen ::bitwise-and [instr loc-mapper]
  (codegen-commutative-binop "and" instr loc-mapper))

(defmethod ir/codegen ::bitwise-or [instr loc-mapper]
  (codegen-commutative-binop "or" instr loc-mapper))

(defmethod ir/codegen ::bitwise-xor [instr loc-mapper]
  (codegen-commutative-binop "xor" instr loc-mapper))

(defmethod ir/codegen ::bitwise-not [instr loc-mapper]
  (codegen-unop "not" instr loc-mapper))

(defmethod ir/codegen ::bool-not [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::ir/helper)]
    [(make-code "mov" a-loc h-loc)
     "cmp $0, " (ir/read-location h-loc)
     (make-code "setz " (assoc h-loc ::ir/size 8))
     (make-code "movz" (assoc h-loc ::ir/size 8) t-loc)]))

(defn- codegen-shift [name instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::ir/helper)]
    (cond
      (and (= a-loc t-loc) (ir/not-both-memory a-loc b-loc))
      [(make-code name b-loc a-loc)] 
      :else
      [(make-code "mov" a-loc h-loc)
       (make-code name b-loc h-loc)
       (make-code "mov" h-loc t-loc)])))

(defmethod ir/codegen ::shift-left [instr loc-mapper]
  (codegen-shift "sal" instr loc-mapper))

(defmethod ir/codegen ::shift-right [instr loc-mapper]
  (codegen-shift "sar" instr loc-mapper))