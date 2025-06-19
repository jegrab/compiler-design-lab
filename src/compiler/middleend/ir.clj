(ns compiler.middleend.ir
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(s/def ::instruction-name keyword?)
;(s/def ::instruction (s/cat ::instruction-name))



; rval is constant or var or [op params...]
; [::assign var r-val]

(def start
  [".global my_main"
   ".text"
   
   "my_main:"])

(defmulti get-res-var-name first)
(defmethod get-res-var-name ::assign [[assign dest input]] dest)
(defmethod get-res-var-name :default [_] nil)

(defn enumerate-vars [ir-vec]
  (let [var-set (into #{} (map get-res-var-name ir-vec))
        with-nums (map (fn [var id] [var id]) var-set (drop 1 (range)))]
    (into {} with-nums)))

(defmulti codegen (fn [instr var-ids] (first instr)))

(defn read-stack [offset]
  (str " -" offset "(%rbp)"))

(defmethod codegen ::return [ret var-ids]
  [(str "movl " (read-stack (* 4 (var-ids ::ret-register)))  ", %eax")
   "movslq %eax, %rdi #return"
   "leave             #|"
   "ret               #|"
   ""])

(defmethod codegen ::nop [[nop] var-ids]
  [(str "nop")])

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

(defmethod codegen ::if-equal-jmp [[if-eq-jmp a b target] var-ids]
  (let [a (* 4 (var-ids a))
        b (* 4 (var-ids b))]
    [(str "movl " (read-stack a) ", %eax " " # if eq jmp")
     (str "cmp " "%eax" ", " (read-stack b) )
     (str "je " target)]))

(defmethod codegen ::if-greater-jmp [[if-eq-jmp a b target] var-ids]
  (let [a (* 4 (var-ids a))
        b (* 4 (var-ids b))]
    [(str "movl " (read-stack b) ", %eax " " # if gt jmp")
     (str "cmp " "%eax" ", " (read-stack a))
     (str "jg " target)]))

(defmethod codegen ::if-greater-or-equal-jmp [[if-eq-jmp a b target] var-ids]
  (let [a (* 4 (var-ids a))
        b (* 4 (var-ids b))]
    [(str "movl " (read-stack b) ", %eax " " # if geq jmp")
     (str "cmp " "%eax" ", " (read-stack a))
     (str "jge " target)]))

(defn fun-id-to-sym [fun-id]
  (str/replace (str "fun_" fun-id) #"[:-]" ""))

(def param-register-sequence ["edi" "esi" "edx" "r8d" "r9d"])

(defn put-args [tmps var-ids]
  (loop [res []
         count-done 0
         put-on-stack 0
         tmps tmps]
    (cond 
      (and (empty? tmps)
           (zero? (mod put-on-stack 2)))
      [res (* 8 put-on-stack)]
      
      (empty? tmps)
      (recur (conj res "pushq $0")
             count-done
             (inc put-on-stack)
             tmps)
      
      (< count-done (count param-register-sequence))
      (recur (conj res (str "movl " 
                            (read-stack (* 4 (var-ids (first tmps))))
                            ", %" (nth param-register-sequence count-done)))
             (inc count-done)
             put-on-stack
             (rest tmps))
      :else
      (recur (conj res (str "push "
                            (read-stack (* 4 (var-ids (first tmps))))))
             count-done
             (inc put-on-stack)
             (rest tmps)))))

(defn codegen-assign-call [[call fun-id & tmps] dest-offset var-ids]
  (let [[put-args-code additional-stack-size] (put-args tmps var-ids)]
    (into
     put-args-code
     [(str "call " (fun-id-to-sym fun-id))
      (str "add $" additional-stack-size ", %rsp")
      (str "movl %eax, " (read-stack dest-offset))])))

(def print-code
  [(str (fun-id-to-sym :print) ":")
   "pushq %rbp"
   "movq %rsp, %rbp"
   "call putchar"
   "leave"
   "ret"
   ""])

(def read-code
  [(str (fun-id-to-sym :read) ":")
   "pushq %rbp"
   "movq %rsp, %rbp"
   "call getchar"
   "leave"
   "ret"
   ""])

(def flush-code
  [(str (fun-id-to-sym :flush) ":")
   "pushq %rbp"
   "movq %rsp, %rbp"
   "movq stdout(%rip), %rdi"
   "call fflush"
   "leave"
   "ret"
   ""])

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
           (= ::bit-not (first input)))
      (let [source-offset (* 4 (var-ids (second input)))]
        [(str "movl " (read-stack source-offset) ", %eax" " # " dest " = -" input)
         "notl %eax"
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
           (= ::shift-left (first input)))
      (let [left-offset (* 4 (var-ids (nth input 1)))
            right-offset (* 4 (var-ids (nth input 2)))]
        [(str "movl " (read-stack left-offset) ", %eax")
         (str "movl " (read-stack right-offset) ", %ecx")
         (str "sall %cl, %eax")
         (str "movl %eax, " (read-stack dest-offset))])

      (and (vector? input)
           (= ::shift-right (first input)))
      (let [left-offset (* 4 (var-ids (nth input 1)))
            right-offset (* 4 (var-ids (nth input 2)))]
        [(str "movl " (read-stack left-offset) ", %eax")
         (str "movl " (read-stack right-offset) ", %ecx")
         (str "sarl %cl, %eax")
         (str "movl %eax, " (read-stack dest-offset))])

      (and (vector? input)
           (#{::plus ::minus ::mul ::bit-and ::bit-or ::bit-xor} (first input)))
      (let [left-offset (* 4 (var-ids (nth input 1)))
            right-offset (* 4 (var-ids (nth input 2)))]
        [(str "movl " (read-stack left-offset) ", %eax")
         (str "movl " (read-stack right-offset) ", %ebx")
         (str ({::plus "addl" ::minus "subl" ::mul "imull" ::bit-and "andl" ::bit-or "orl" ::bit-xor "xorl"} 
               (first input)) " " "%ebx, %eax")
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
         (str "movl %edx, " (read-stack dest-offset))])
      (and (vector? input)
           (= ::call (first input)))
      (codegen-assign-call input dest-offset var-ids))))

(defn align-to-16-bytes [needed-bytes]
  (let [rem (rem needed-bytes 16)]
    (if (zero? rem)
      needed-bytes
      (+ needed-bytes (- 16 rem)))))

(defn make-code [instrs]
  (let [num-vars (enumerate-vars instrs)
        asm-lines (apply concat (map #(codegen % num-vars) instrs))
        preamble (concat print-code read-code flush-code)
        before start
        stack-bytes (align-to-16-bytes (* 4 (count (enumerate-vars instrs))))
        make-stack ["pushq %rbp"
                    "movq %rsp, %rbp"
                    (str "subq $" stack-bytes ", %rsp")]
        after ["\n"]
        whole (concat preamble before make-stack asm-lines after)]
    (clojure.string/join "\n" whole)))