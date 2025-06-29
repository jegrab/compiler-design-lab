(ns compiler.middleend.ir 
  (:require
    [compiler.middleend.ir :as ir]))

; instruction { ::kind ::assign ::dest name ::source name}
; 
;
;

(defmulti codegen
  "generates a list of strings. each string is a line of assembly.
   location-mapper is  a function that takes an identifier and returns a location.
   an integer literal can also be instead of an identifier and represents a location that stores that literal identifier (but can not be written to).
   the identifier ::helper returns a register that is not used by any other identifier.
   A location is a map of form {::kind kind ::size bytes} with additional fields depending on the kind
   where kind is something like ::register or ::stack
   and ::size is the size of the data in the location in bits"
  (fn [instr location-mapper] (::kind instr)))

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

(defmulti read-location
  "takes an location and returns a string (asm code) that represents that location.
   This string is a register or a relative position. "
  (fn [loc] (::kind loc)))

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


(defn move [a target] {::kind ::move ::a a ::target target})
(defmethod codegen ::move [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        t-loc (loc-mapper (::target instr))
        h-loc (loc-mapper ::helper)]
    (if (not-both-memory a-loc bit-shift-left)
      [(str "mov" (size-suffix a-loc) " " (read-location a-loc) ", " (read-location t-loc))]
      [(str "mov" (size-suffix a-loc) "")])))
