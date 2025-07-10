(ns compiler.middleend.ir
  (:require
   [compiler.frontend.common.id :as id]
   [clojure.string :as str]))

; instruction { ::kind ::assign ::dest name ::source name}
; 
;
; block { ::name label ::code [instructions ...] ::cont ::cont}
; cont { ::kind goto ::target label}
; cont { ::kind if-then-else ::test-var ::target label}
; function { ::name label ::params [params...] ::start ::label ::blocks {name block ...}

(defn make-name
  ([size]
   {::kind ::name
    ::id (id/make-tmp)
    ::size size})
  ([size name]
   (assoc (make-name size) ::name name)))

(defn make-name-with-same-size
  ([other-id]
   {::kind ::name
    ::id (id/make-tmp)
    ::size (::size other-id)})
  ([other-id name] 
   (assoc (make-name-with-same-size other-id) ::name name)))

(defn make-constant [size value]
  {::kind ::constant
   ::value value
   ::size size})

(def ^:dynamic architecture ::x86-64)

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

(defn fun-block
  "takes a name and returns an unfinished function block.
   i has an empty ::blocks map and an unfinished ::current block that contains no code."
  [name params return-size]
  (let [start-label name]
    {::name name
     ::params params
     ::start start-label
     ::blocks {start-label {::name start-label
                            ::code []}}
     ::current start-label
     ::return-size return-size}))

(defn add-instruction
  "adds the given instruction to the end of the block with the specified name.
   The default is the name stored in ::current of the block.
   If there is neither a block-name provided nor a ::current in the block, then the instruction is not added."
  ([fun-block instr] (add-instruction fun-block instr (::current fun-block)))
  ([fun-block instr block-name]
   (let [current ((::blocks fun-block) block-name)
         new-code (conj (::code current) instr)
         new-curr (assoc current ::code new-code)]
     (if current
       (assoc-in fun-block
                 [::blocks block-name] new-curr)
       fun-block))))

(defn set-cont
  "sets the given cont as the cont of the block with the specified name.
   The default is the name stored in ::current of the block.
   Removes ::current from the block"
  ([fun-block cont] (set-cont fun-block cont (::current fun-block)))
  ([fun-block cont block-name]
   (let [current ((::blocks fun-block) block-name)
         new-curr (assoc current ::cont cont)]
     (dissoc (assoc-in fun-block
                       [::blocks block-name] new-curr)
             ::current))))

(defmulti codegen-instruction
  "generates a list of strings. each string is a line of assembly.
   location-mapper is  a function that takes an identifier and returns a location.
   an integer literal can also be instead of an identifier and represents a location that stores that literal identifier (but can not be written to).
   the identifier ::helper returns a register that is not used by any other identifier.
   The identifier ::ret returns a location where the return value should be stored in.
   A location is a map of form {::kind kind ::size bytes} with additional fields depending on the kind
   where kind is something like ::register or ::stack
   and ::size is the size of the data in the location in bits"
  (fn [instr location-mapper] [(::kind instr) architecture]))

(defn label-string [label]
  (let [name (str/replace (str label) #"[^a-zA-Z0-9]" "")]
    (str name "_" (::id label))))

(defmulti codegen-continuation (fn [cont loc-mapper] [(::kind cont) architecture]))

(defn- codegen-block [block loc-mapper]
  "generates a list of strings. each string is a line of assembly.
   location-mapper is  a function that takes an identifier and returns a location.
   an integer literal can also be instead of an identifier and represents a location that stores that literal identifier (but can not be written to).
   the identifier ::helper returns a register that is not used by any other identifier.
   The identifier ::ret returns a location where the return value should be stored in.
   A location is a map of form {::kind kind ::size bytes} with additional fields depending on the kind
   where kind is something like ::register or ::stack
   and ::size is the size of the data in the location in bits"
  (apply concat
         (conj (mapv #(codegen-instruction % loc-mapper) (::code block))
               (codegen-continuation (::cont block) loc-mapper))))

(defmulti codegen-function (fn [fun-block] architecture))
(defmulti codegen (fn [fun-blocks main-id] architecture))

(defmulti read-location
  "takes an location and returns a string (asm code) that represents that location.
   This string is a register or a relative position. "
  (fn [loc] (::kind loc)))


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

(defmethod read-location ::register [loc]
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

(defn loc-of-stack [offset size]
  {::kind ::stack
   ::size size
   ::offset offset})

(defmethod read-location ::stack [loc]
  (str " -" (::offset loc) "(%rsp)"))

(defmethod read-location ::constant [loc]
  (str "$" (::value loc)))

(defmulti get-res-var-name (fn [a] (::kind a)))
(defmethod get-res-var-name :default [a] (::target a))

(defn fun-instructions [fun-block]
  (apply concat (map (fn [[label {code ::code}]] code) (::blocks fun-block))))

(defn create-stack-loc-mapper [fun-block]
  (loop [instructions (fun-instructions fun-block)
         offset 0
         seen #{}
         var-mapper {::helper (loc-of-reg ::accumulator 32)}]
    (if (empty? instructions)
      (fn [id]
        (cond
          (= ::ret id)
          (loc-of-reg ::accumulator (::return-size fun-block))

          (= ::helper id)
          (loc-of-reg ::accumulator (::return-size fun-block))

          :else
          (case (::kind id)
            ::constant {::kind ::constant
                        ::value (::value id)
                        ::size (::size id)}
            ::name (var-mapper (::id id)))))
      (let [curr (first instructions)
            target (::target curr)
            size (::size target)]
        (if (or (= ::ret target) (seen target))
          (recur (rest instructions)
                 offset
                 seen
                 var-mapper)

          (let [size-bytes (case size
                             8 1
                             16 2
                             32 4
                             64 8)
                new-offset (+ offset size-bytes)]
            (recur (rest instructions)
                   new-offset
                   (conj seen target)
                   (assoc var-mapper ::target (loc-of-stack new-offset size)))))))))


(defmethod codegen-function ::x86-64 [fun-block]
  (let [loc-mapper-stack (create-stack-loc-mapper fun-block)
        block-code (map (fn [[name block]] (codegen-block block loc-mapper-stack))
                        (::blocks fun-block))]
    (into [(str (label-string (::name fun-block)) ":")
           "pushq %rbp"
           "movq %rsp, %rbp"]
          (apply concat block-code))))

(def print-code
  [(str (label-string :print) ":")
   "pushq %rbp"
   "movq %rsp, %rbp"
   "call putchar"
   "movl $0, %eax"
   "leave"
   "ret"
   ""])

(def read-code
  [(str (label-string :read) ":")
   "pushq %rbp"
   "movq %rsp, %rbp"
   "call getchar"
   "leave"
   "ret"
   ""])

(def flush-code
  [(str (label-string :flush) ":")
   "pushq %rbp"
   "movq %rsp, %rbp"
   "movq stdout(%rip), %rdi"
   "call fflush"
   "movl $0, %eax"
   "leave"
   "ret"
   ""])

(defmethod codegen ::x86-64 [fun-blocks main-id]
  (let [decl-asm (apply concat (map codegen-function fun-blocks))
        preamble (concat print-code read-code flush-code)
        before [".global my_main"
                ".text"
                "\n"
                "my_main:"
                "pushq %rbp"
                "movq %rsp, %rbp"
                (str "call " (label-string main-id))
                "movl %eax, %ebx"
                "movq stdout(%rip), %rdi"
                "call fflush"
                "movl %ebx, %eax"
                "leave"
                "ret"
                "\n"
                "\n"]
  
        after ["\n"]
        whole (concat preamble before decl-asm after)]
    (clojure.string/join "\n" whole)))

(defmethod codegen-continuation [::goto ::x86-64] [goto loc-mapper] 
  [(str "jmp " (label-string (::target goto)))])

(defmethod codegen-continuation [::return ::x86-64] [ret loc-mapper]
  ["leave"
   "ret"])

(defmethod codegen-continuation [::if-then-else ::x86-64] [ite loc-mapper]
  (let [test-loc (loc-mapper (::test ite))
        test-loc-code (read-location test-loc)]
    [(str "cmp $0, " test-loc-code)
     (str "jne " (label-string (::then ite)))
     (str "jmp " (label-string (::else ite)))]))

(defmethod codegen-instruction [::move ::x86-64] [instr loc-mapper]
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

(defmethod codegen-instruction [::negate ::x86-64] [instr loc-mapper]
  (codegen-unop "neg" instr loc-mapper))

(defmethod codegen-instruction [::add ::x86-64] [instr loc-mapper]
  (codegen-commutative-binop "add" instr loc-mapper))

(defmethod codegen-instruction [::sub ::x86-64] [instr loc-mapper]
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

(defmethod codegen-instruction [::mul ::x86-64] [instr loc-mapper]
  (let [a-loc (loc-mapper (::a instr))
        b-loc (loc-mapper (::b instr))
        t-loc (loc-mapper (::target instr))]
    [(make-code "imul" a-loc b-loc t-loc)]))


(defmethod special-register? ::accumulator [_] true)
(defmethod special-register? ::data [_] true)

(defmethod codegen-instruction [::div ::x86-64] [instr loc-mapper]
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

(defmethod codegen-instruction [::mod ::x86-64] [instr loc-mapper]
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


(defmethod codegen-instruction [::bitwise-and ::x86-64] [instr loc-mapper]
  (codegen-commutative-binop "and" instr loc-mapper))

(defmethod codegen-instruction [::bitwise-or ::x86-64] [instr loc-mapper]
  (codegen-commutative-binop "or" instr loc-mapper))

(defmethod codegen-instruction [::bitwise-xor ::x86-64] [instr loc-mapper]
  (codegen-commutative-binop "xor" instr loc-mapper))

(defmethod codegen-instruction [::bitwise-not ::x86-64] [instr loc-mapper]
  (codegen-unop "not" instr loc-mapper))

(defmethod codegen-instruction [::bool-not ::x86-64] [instr loc-mapper]
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

(defmethod codegen-instruction [::shift-left ::x86-64] [instr loc-mapper]
  (codegen-shift "sal" instr loc-mapper))

(defmethod codegen-instruction [::shift-right ::x86-64] [instr loc-mapper]
  (codegen-shift "sar" instr loc-mapper))