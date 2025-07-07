(ns compiler.middleend.ir
  (:require
   [compiler.frontend.common.id :as id]
   [clojure.string :as str]))

; instruction { ::kind ::assign ::dest name ::source name}
; 
;
; block { ::name label ::code [instructions ...] ::next ::cont}
; cont { ::kind goto ::target label}
; cont { ::kind if-then-else ::test-var ::target label}
; function { ::name label ::params [params...] ::start ::label ::blocks {name block ...}

(defn mk-fn-block
  "produces an unfinished function with the given names and parameters but no blocks.
   Has an empty ::current-block that can be used to construct a complete the function."
  [name params]
  {::name name
   ::params (into [] params)
   ::start ::start
   ::current-block {::name ::start ::code []}})


(defmulti codegen-instruction
  "generates a list of strings. each string is a line of assembly.
   location-mapper is  a function that takes an identifier and returns a location.
   an integer literal can also be instead of an identifier and represents a location that stores that literal identifier (but can not be written to).
   the identifier ::helper returns a register that is not used by any other identifier.
   A location is a map of form {::kind kind ::size bytes} with additional fields depending on the kind
   where kind is something like ::register or ::stack
   and ::size is the size of the data in the location in bits"
  (fn [instr location-mapper] (::kind instr)))

(defn label [label-name]
  {::kind ::label
   ::id (id/make-id-num)
   ::name label-name})

(defn label-string [label]
  (let [name (str/replace (::name label) #"[^a-zA-Z0-9]" "")]
    (str name "_" (::id label))))

(defmulti codegen-continuation (fn [cont loc-mapper] (::kind cont)))

(defn codegen-block [block loc-mapper]
  ())

(defmulti read-location
  "takes an location and returns a string (asm code) that represents that location.
   This string is a register or a relative position. "
  (fn [loc] (::kind loc)))


(defn if-then-else [test-bool-location then-label else-label]
  {::kind ::if-then-else
   ::test test-bool-location
   ::then then-label
   ::else else-label})

(defn goto [target-label]
  {::kind ::goto
   ::target target-label})

(defn return []
  {::kind ::return})

(defn un-op [op-name source target]
  {::kind op-name
   ::a source
   ::target target})

(defn bin-op [op-name a b target]
  {::kind op-name
   ::a a
   ::b b
   ::target target})

(defn move [a target] (un-op ::move a target))





(defmulti memory? (fn [loc] (::kind loc)))
(defmethod memory? :default [_] false)
(defmethod memory? ::memory [reg] true)

(defn not-both-memory [a b]
  (not (and (memory? a) (memory? b))))

(defn size-suffix [loc]
  (case (::size loc)
    8 "b"
    16 "w"
    32 "l"
    64 "q"))

(defn loc-of-reg [reg-name size]
  {::kind ::register
   ::size size
   ::name reg-name})



(def extra-register #{::reg-8 ::reg-9 ::reg-10 ::reg-11 ::reg-12 ::reg-13 ::reg-14 ::reg-15})

(defmethod read-location :register [loc]
  (let [prefix (if (extra-register (::name loc))
                 "r"
                 (case (::size loc)
                   8 ""
                   16 ""
                   32 "e"
                   64 "r"
                   (throw (Exception. (str "unsupported location size of " (::size loc) " bit")))))
        postfix (if (extra-register (::name loc))
                  (case (::size loc)
                    8 "b"
                    16 "w"
                    32 "d"
                    64 ""
                    (throw (Exception. (str "unsupported location size of " (::size loc) " bit"))))
                  (case (::size loc)
                    8 "l"
                    ""))
        reg-name (case (::name loc)
                   ::accumulator (if (= 8 (::size loc)) "a" "ax")
                   ::base (if (= 8 (::size loc)) "b" "bx")
                   ::counter (if (= 8 (::size loc)) "c" "cx")
                   ::stack-pointer "sp"
                   ::stack-base "bp"
                   ::destination-index "di"
                   ::source-index "si"
                   ::data (if (= 8 (::size loc)) "d" "dx")
                   ::reg-8 "8"
                   ::reg-9 "9"
                   ::reg-10 "10"
                   ::reg-11 "11"
                   ::reg-12 "12"
                   ::reg-13 "13"
                   ::reg-14 "14"
                   ::reg-15 "15"
                   (throw (Exception. (str "unknow register name " (::name loc)))))]
    (str "%" prefix reg-name postfix)))

(defmulti special-register?
  "takes a register name (keyword).
   If the register can be used by the allocator for some purpose, returns false.
   If true is returned, then some instruction uses the register for special purposes.
   It then may overwrite the value of the register and thus the register should not be used by the allocator.
   But it may be used as the ::helper register by the allocator."
  identity)
(defmethod special-register? :default [_] false)

(defmethod codegen-continuation ::goto [goto loc-mapper]
  [(str "jmp " (label-string (::target goto)))])

(defmethod codegen-continuation ::if-then-else [ite loc-mapper]
  (let [test-loc (loc-mapper (::test ite))
        test-loc-code (read-location test-loc)]
    [(str "cmp $0, " test-loc-code)
     (str "jne " (label-string (::then ite)))
     (str "jmp " (label-string (::else ite)))]))

(defmethod codegen-instruction ::move [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::helper)]
    (if (not-both-memory a-loc bit-shift-left)
      [(str "mov" (size-suffix a-loc) " " (read-location a-loc) ", " (read-location t-loc))]
      [(str "mov" (size-suffix a-loc) "")])))



(defn- make-code [asm-instr & operand-locs]
  (let [suffix (size-suffix (first operand-locs))]
    (apply str asm-instr suffix " " (clojure.string/join ", " (map read-location operand-locs)))))



(defn- codegen-commutative-binop [name instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::helper)]
    (cond
      (and (= a-loc t-loc) (not-both-memory a-loc b-loc))
      [(make-code name b-loc a-loc)]
      (and (= b-loc t-loc) (not-both-memory a-loc b-loc))
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

(defmethod codegen-instruction ::negate [instr loc-mapper]
  (codegen-unop "neg" instr loc-mapper))

(defmethod codegen-instruction ::add [instr loc-mapper]
  (codegen-commutative-binop "add" instr loc-mapper))

(defmethod codegen-instruction ::sub [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::helper)]
    (cond
      (and (= a-loc t-loc) (not-both-memory a-loc b-loc))
      [(make-code "sub" b-loc a-loc)]
      :else
      [(make-code "mov" a-loc h-loc)
       (make-code "sub" b-loc h-loc)
       (make-code "mov" h-loc t-loc)])))

(defmethod codegen-instruction ::mul [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))]
    [(make-code "imul" a-loc b-loc t-loc)]))


(defmethod special-register? ::accumulator [_] true)
(defmethod special-register? ::data [_] true)

(defmethod codegen-instruction ::div [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        size (::size a-loc)]
    (case size
      16 (throw (Exception. "16 bit division not implemented"))
      32 [(make-code "mov" a-loc (loc-of-reg ::accumulator size))
          "cdq"
          (make-code "idiv" b-loc)
          (make-code "mov" (loc-of-reg ::accumulator size) t-loc)]
      64 (throw (Exception. "64 bit division not implemented")))))

(defmethod codegen-instruction ::mod [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        size (::size a-loc)]
    (case size
      16 (throw (Exception. "16 bit division not implemented"))
      32 [(make-code "mov" a-loc (loc-of-reg ::accumulator size))
          "cdq"
          (make-code "idiv" b-loc)
          (make-code "mov" (loc-of-reg ::data size) t-loc)]
      64 (throw (Exception. "64 bit division not implemented")))))


(defmethod codegen-instruction ::bitwise-and [instr loc-mapper]
  (codegen-commutative-binop "and" instr loc-mapper))

(defmethod codegen-instruction ::bitwise-or [instr loc-mapper]
  (codegen-commutative-binop "or" instr loc-mapper))

(defmethod codegen-instruction ::bitwise-xor [instr loc-mapper]
  (codegen-commutative-binop "xor" instr loc-mapper))

(defmethod codegen-instruction ::bitwise-not [instr loc-mapper]
  (codegen-unop "not" instr loc-mapper))

(defmethod codegen-instruction ::bool-not [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::helper)]
    [(make-code "mov" a-loc h-loc)
     "cmp $0, " (read-location h-loc)
     (make-code "setz " (assoc h-loc ::size 8))
     (make-code "movz" (assoc h-loc ::size 8) t-loc)]))

(defn- codegen-shift [name instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::helper)]
    (cond
      (and (= a-loc t-loc) (not-both-memory a-loc b-loc))
      [(make-code name b-loc a-loc)]
      :else
      [(make-code "mov" a-loc h-loc)
       (make-code name b-loc h-loc)
       (make-code "mov" h-loc t-loc)])))

(defmethod codegen-instruction ::shift-left [instr loc-mapper]
  (codegen-shift "sal" instr loc-mapper))

(defmethod codegen-instruction ::shift-right [instr loc-mapper]
  (codegen-shift "sar" instr loc-mapper))