(ns compiler.frontend.integer
  (:require
   [compiler.frontend.common.id :as id]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.common.error :as err]
   [compiler.frontend.common.type :as type]
   [compiler.frontend.expression :as expr]
   [compiler.middleend.ir :as ir]))

(def int-type (type/simple-type ::integer))
(defmethod type/size-in-bit ::integer [_] 32)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule type/parse ::integer
  [i (token ::lex/int)]
  int-type)

(p/def-op expr/parse-expr numerical-constant
  [i (token ::lex/numerical-constant)]
  {::ast/kind ::numerical-constant
   ::ast/children []
   ::num-kind (case (::lex/num-kind i)
                ::lex/hex ::hex
                ::lex/dec ::dec)
   ::value (::lex/value i)})

(defmethod ast/pretty-print ::numerical-constant [c] (str (::value c)))

(defmethod ast/check-after-parse ::numerical-constant [c]
  (cond
    (and (= ::dec (::num-kind c)) (not (<= 0 (::value c) 2147483648)))
    (err/add-error c (err/make-semantic-error (str "the dec number " (::value c) " does not fit into the range of int.")))

    (and (= ::hex (::num-kind c)) (not (<= 0 (::value c) 0xffffffff)))
    (err/add-error c (err/make-semantic-error (str "the hex number " (::value c) " does not fit into the range of int.")))

    :else c))

(defmethod expr/to-ir ::numerical-constant [c into]
  (ir/move (::value c) into))

(defmethod ast/gen-ir ::numerical-constant [state const]
  (ir/add-instruction state (ir/move (ir/make-constant 32 (::value const)) (::ir/target state))))

(defmethod expr/typecheck ::numerical-constant [c _]
  (assoc c ::type/type int-type))


(p/def-op expr/parse-expr unary-minus
  {:precedence 13}
  [_ (token ::lex/minus)
   e expr/parse-expr]
  {::ast/kind ::unary-minus
   ::ast/children [::child]
   ::child e})

(defmethod ast/pretty-print ::unary-minus [n]
  (str "(-" (ast/pretty-print (::child n)) ")"))

(defmethod expr/to-ir ::unary-minus [n target]
  (let [prev (expr/to-ir (::child n) target)]
    (into prev
          (ir/un-op ::ir/negate target target))))

(defn- gen-ir-unop [state node op]
  (ir/add-instruction (ast/gen-ir state (::child node))
                      (ir/un-op op (::ir/target state) (::ir/target state))))

(defmethod ast/gen-ir ::unary-minus [state minus]
  (gen-ir-unop state minus ::ir/negate))

(defmethod expr/typecheck ::unary-minus [n env]
  (let [new-c (expr/typecheck (::child n) env)
        t (::type/type new-c)
        new-n (assoc n
                     ::type/type int-type
                     ::child new-c)]
    (if (type/equals t int-type)
      new-n
      (err/add-error new-n
                     (err/make-semantic-error (str "type mismatch: unary minus requires int but got " t))))))


(defn bin-op-node [op left right]
  {::ast/kind op
   ::ast/children [::left ::right]
   ::left left
   ::right right})

(defn- pretty-print-binop [node op-str]
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print (::right node)) ")"))

(defn- to-ir-bin-op-old [n target op]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        l-prev (expr/to-ir (::left n) l)
        r-prev (expr/to-ir (::right n) r)]
    (into []
          (concat l-prev
                  r-prev
                  (ir/bin-op op l r target)))))

(defn- to-ir-bin-op [node state op]
  (let [t (::ir/target state)
        l (ir/make-name 32)
        r (ir/make-name 32)
        state (ast/gen-ir (assoc state ::ir/target l) (::left node))
        state (ast/gen-ir (assoc state ::ir/target r) (::right node))]
    (ir/add-instruction (assoc state ::ir/target t) (ir/bin-op op l r t))))


(defn- typecheck-bin-op [op env]
  (let [ta  (expr/typecheck (::left op) env)
        tb (expr/typecheck (::right op) env)]
    (if (and (type/equals (::type/type ta) (::type/type tb))
             (type/equals (::type/type ta) int-type)
             (type/equals (::type/type tb) int-type))
      (assoc op
             ::type/type int-type
             ::left ta
             ::right tb)
      (assoc
       (err/add-error op (err/make-semantic-error (str "type mismatch between " ta " and " tb " that should both be " int-type)))
       ::type/type type/error))))


(p/def-op expr/parse-expr plus
  {:precedence 11 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/plus)
   right expr/parse-expr]
  (bin-op-node ::plus left right))

(defmethod ast/pretty-print ::plus [n] (pretty-print-binop n "+"))

(defmethod expr/to-ir ::plus [n res] (to-ir-bin-op-old n res ::ir/add))
(defmethod ast/gen-ir ::plus [state node] (to-ir-bin-op node state ::ir/add))

(defmethod expr/typecheck ::plus [p env] (typecheck-bin-op p env))


(p/def-op expr/parse-expr minus
  {:precedence 11 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/minus)
   right expr/parse-expr]
  (bin-op-node ::minus left right))

(defmethod ast/pretty-print ::minus [n] (pretty-print-binop n "-"))

(defmethod expr/to-ir ::minus [n res] (to-ir-bin-op-old n res ::ir/sub))
(defmethod ast/gen-ir ::minus [state node] (to-ir-bin-op node state ::ir/sub))

(defmethod expr/typecheck ::minus [p env] (typecheck-bin-op p env))


(p/def-op expr/parse-expr mul
  {:precedence 12 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/mul)
   right expr/parse-expr]
  (bin-op-node ::mul left right))

(defmethod ast/pretty-print ::mul [n] (pretty-print-binop n "*"))

(defmethod expr/to-ir ::mul [n res] (to-ir-bin-op-old n res ::ir/mul))
(defmethod ast/gen-ir ::mul [state node] (to-ir-bin-op node state ::ir/mul))

(defmethod expr/typecheck ::mul [p env] (typecheck-bin-op p env))

(p/def-op expr/parse-expr div
  {:precedence 12 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/div)
   right expr/parse-expr]
  (bin-op-node ::div left right))

(defmethod ast/pretty-print ::div [n] (pretty-print-binop n "/"))

(defmethod expr/to-ir ::div [n res] (to-ir-bin-op-old n res ::ir/div))
(defmethod ast/gen-ir ::div [state node] (to-ir-bin-op node state ::ir/div))

(defmethod expr/typecheck ::div [p env] (typecheck-bin-op p env))

(p/def-op expr/parse-expr mod
  {:precedence 12 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/mod)
   right expr/parse-expr]
  (bin-op-node ::mod left right))

(defmethod ast/pretty-print ::mod [n] (pretty-print-binop n "%"))

(defmethod expr/to-ir ::mod [n res] (to-ir-bin-op-old n res ::ir/mod))
(defmethod ast/gen-ir ::mod [state node] (to-ir-bin-op node state ::ir/mod))

(defmethod expr/typecheck ::mod [p env] (typecheck-bin-op p env))


(p/def-op expr/parse-expr shift-left
  {:precedence 10 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/shift-left)
   right expr/parse-expr]
  (bin-op-node ::shift-left left right))

(defmethod ast/pretty-print ::shift-left [n] (pretty-print-binop n "<<"))

(defmethod expr/to-ir ::shift-left [n res] (to-ir-bin-op-old n res ::ir/shift-left))
(defmethod ast/gen-ir ::shift-left [state node] (to-ir-bin-op node state ::ir/shift-left))

(defmethod expr/typecheck ::shift-left [p env] (typecheck-bin-op p env))

(p/def-op expr/parse-expr shift-right
  {:precedence 10 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/shift-right)
   right expr/parse-expr]
  (bin-op-node ::shift-right left right))

(defmethod ast/pretty-print ::shift-right [n] (pretty-print-binop n "<<"))

(defmethod expr/to-ir ::shift-right [n res] (to-ir-bin-op-old n res ::ir/shift-right))
(defmethod ast/gen-ir ::shift-right [state node] (to-ir-bin-op node state ::ir/shift-right))

(defmethod expr/typecheck ::shift-right [p env] (typecheck-bin-op p env))


(p/def-op expr/parse-expr bitwise-not
  {:precedence 13}
  [_ (token ::lex/bit-not)
   e expr/parse-expr]
  {::ast/kind ::bit-not
   ::ast/children [::child]
   ::child e})

(defmethod ast/pretty-print ::bit-not [n]
  (str "(~" (ast/pretty-print (::child n)) ")"))

(defmethod expr/to-ir ::bit-not [n target]
  (let [tmp (id/make-tmp)
        prev (expr/to-ir (::child n) tmp)]
    (into prev
          (ir/un-op ::ir/bitwise-not tmp target))))

(defmethod ast/gen-ir ::bit-not [state node]
  (gen-ir-unop state node ::ir/bitwise-not))

(defmethod expr/typecheck ::bit-not [n env]
  (let [new-c (expr/typecheck (::child n) env)
        t (::type/type new-c)
        new-n (assoc n
                     ::type/type int-type
                     ::child new-c)]
    (if (type/equals t int-type)
      new-n
      (err/add-error new-n
                     (err/make-semantic-error (str "type mismatch: bitwise not requires int but got " t))))))


(p/def-op expr/parse-expr bit-and
  {:precedence 7 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/bit-and)
   right expr/parse-expr]
  (bin-op-node ::bit-and left right))

(defmethod ast/pretty-print ::bit-and [n] (pretty-print-binop n "&"))

(defmethod expr/to-ir ::bit-and [n res] (to-ir-bin-op-old n res ::ir/bitwise-and))
(defmethod ast/gen-ir ::bit-and [state node] (to-ir-bin-op node state ::ir/bitwise-and))

(defmethod expr/typecheck ::bit-and [p env] (typecheck-bin-op p env))



(p/def-op expr/parse-expr bit-xor
  {:precedence 6 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/bit-xor)
   right expr/parse-expr]
  (bin-op-node ::bit-xor left right))

(defmethod ast/pretty-print ::bit-xor [n] (pretty-print-binop n "^"))

(defmethod expr/to-ir ::bit-xor [n res] (to-ir-bin-op-old n res ::ir/bitwise-xor))
(defmethod ast/gen-ir ::bit-xor [state node] (to-ir-bin-op node state ::ir/bitwise-xor))

(defmethod expr/typecheck ::bit-xor [p env] (typecheck-bin-op p env))



(p/def-op expr/parse-expr bit-or
  {:precedence 5 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/bit-or)
   right expr/parse-expr]
  (bin-op-node ::bit-or left right))

(defmethod ast/pretty-print ::bit-or [n] (pretty-print-binop n "|"))

(defmethod expr/to-ir ::bit-or [n res] (to-ir-bin-op-old n res ::ir/bitwise-or))
(defmethod ast/gen-ir ::bit-or [state node] (to-ir-bin-op node state ::ir/bitwise-or))

(defmethod expr/typecheck ::bit-or [p env] (typecheck-bin-op p env))