(ns compiler.middleend.ir)

; instruction { ::kind ::assign ::dest name ::source name}
; 
;
;

(defmulti codegen
  "generates a list of strings. each string is a line of assembly.
   location-mapper is  a function that takes an identifier and returns a location.
   the identifier ::helper returns a register that is not used by any other identifier.
   A location is a map of form {::kind kind ::size bytes} with additional fields depending on the kind
   where kind is something like ::register or ::stack
   and ::size is the size of the data in the location in bits"
  (fn [instr location-mapper] (::kind instr)))

(defmulti memory? (fn [loc] (::kind loc)))
(defmethod memory? :default [_] false)
(defmethod memory? ::memory [reg] true)

(defn size-suffix [loc]
  (case (::size loc)
    16 "w"
    32 "l"
    64 "q"))

(defn int-loc-reg [reg-name]
  {::kind ::register
   ::size 32
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
                   16 ""
                   32 "e"
                   64 "r"
                   (throw (Exception. (str "unsupported location size of " (::size loc) " bit")))))
        postfix (if (extra-register (::name loc))
                  (case (::size loc)
                    16 "w"
                    32 "d"
                    64 ""
                    (throw (Exception. (str "unsupported location size of " (::size loc) " bit"))))
                  "")
        reg-name (case (::name loc)
                   ::accumulator "ax"
                   ::base "bx"
                   ::counter "cx"
                   ::stack-pointer "sp"
                   ::stack-base "bp"
                   ::destination-index "di"
                   ::source-index "si"
                   ::data "dx"
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
