(ns compiler.middleend.ir
  (:require [clojure.spec.alpha :as s]))

(s/def ::instruction-name keyword?)
;(s/def ::instruction (s/cat ::instruction-name))



; rval is constant or var or [op params...]
; [::assign var r-val]

(def start
  [".global main"
   ".global _main"
   ".text"

   "main:"
   "call _main"

   "movq %rax, %rdi" ; move the return value into the first argument for the syscall
   "movq $0x3C, %rax" ; move the exit syscall number into rax
   "syscall"

   "_main:"])

(defmulti get-res-var-name first)
(defmethod get-res-var-name ::assign [[assign dest input]] dest)
(defmethod get-res-var-name ::return [ret] nil)
(defmethod get-res-var-name ::if-false-jmp [ret] nil)
(defmethod get-res-var-name ::if-true-jmp [ret] nil)
(defmethod get-res-var-name ::goto [ret] nil)
(defmethod get-res-var-name ::target [ret] nil)

(defn enumerate-vars [ir-vec]
  (let [var-set (into #{} (map get-res-var-name ir-vec))
        with-nums (map (fn [var id] [var id]) var-set (drop 1 (range)))]
    (into {} with-nums)))

(defmulti codegen (fn [instr var-ids] (first instr)))

(defn read-stack [offset]
  (str " -" offset "(%rsp)"))

(defmethod codegen ::return [ret var-ids]
  [(str "movl " (read-stack (* 4 (var-ids ::ret-register)))  ", %eax")
   "movslq %eax, %rdi #return"
   "leave             #|"
   "ret               #|"
   ""])

(defmethod codegen ::target [[target label] var-ids]
  [(str label ":")])

(defmethod codegen ::goto [[goto label] var-ids]
  [(str "jmp " label)])

(defmethod codegen ::if-true-jmp [[if-true-jmp bool-to-test target] var-ids]
  (let [source-offset (* 4 (var-ids bool-to-test))]
    [(str "movl " (read-stack source-offset) ", %eax" " # if true jmp")
     (str "cmp $0, %eax")
     (str "jne " target)]))

(defmethod codegen ::if-false-jmp [[if-true-jmp bool-to-test target] var-ids]
  (let [source-offset (* 4 (var-ids bool-to-test))]
    [(str "movl " (read-stack source-offset) ", %eax" " # if false jmp")
     (str "cmp $0, %eax")
     (str "je " target)]))

(defmethod codegen ::assign [[assign dest input] var-ids]

  (let [dest-offset (* 4 (var-ids dest))]
    (cond
      (keyword? input)
      [(str "movl " (read-stack (* 4 (var-ids input)))   " , %eax" " # " dest " = " input)
       (str "movl %eax, " (read-stack dest-offset))]

      (integer? input)
      [(str "movl " "$" input  " , " (read-stack dest-offset) " # " dest " = " input)]

      (and (vector? input)
           (= ::negate (first input)))
      (let [source-offset (* 4 (var-ids (second input)))]
        [(str "movl " (read-stack source-offset) ", %eax" " # " dest " = -" input)
         "negl %eax"
         (str "movl %eax, " (read-stack dest-offset))])

      (and (vector? input)
           (= ::not (first input)))
      (let [source-offset (* 4 (var-ids (second input)))]
        [(str "movl " (read-stack source-offset) ", %eax" " # " dest " = !" input)
         "cmp $0, %eax"
         "setz %al"
         "movzbl %al, %eax"
         (str "movl %eax, " (read-stack dest-offset))])

      (and (vector? input)
           (#{::plus ::minus ::mul} (first input)))
      (let [left-offset (* 4 (var-ids (nth input 1)))
            right-offset (* 4 (var-ids (nth input 2)))]
        [(str "movl " (read-stack left-offset) ", %eax")
         (str "movl " (read-stack right-offset) ", %ebx")
         (str ({::plus "addl" ::minus "subl" ::mul "imull"} (first input)) " " "%ebx, %eax")
         (str "movl %eax, " (read-stack dest-offset))])

      (and (vector? input)
           (= ::div (first input)))
      (let [left-offset (* 4 (var-ids (nth input 1)))
            right-offset (* 4 (var-ids (nth input 2)))]
        [(str "movl " (read-stack left-offset) ", %eax")
         "cltd"
         (str "movl " (read-stack right-offset) ", %ebx")
         (str "idivl %ebx")
         (str "movl %eax, " (read-stack dest-offset))])

      (and (vector? input)
           (= ::mod (first input)))
      (let [left-offset (* 4 (var-ids (nth input 1)))
            right-offset (* 4 (var-ids (nth input 2)))]
        [(str "movl " (read-stack left-offset) ", %eax")
         "cltd"
         (str "movl " (read-stack right-offset) ", %ebx")
         (str "idivl %ebx")
         (str "movl %edx, " (read-stack dest-offset))]))))

(defn make-code [instrs]
  (let [num-vars (enumerate-vars instrs)
        asm-lines (apply concat (map #(codegen % num-vars) instrs))
        before start
        make-stack ["pushq %rbp"
                    "movq %rsp, %rbp"
                    (str "subq $" (* 4 (count (enumerate-vars instrs))) ", %rsp")]
        after []
        whole (concat before make-stack asm-lines after)]
    (clojure.string/join "\n" whole)))