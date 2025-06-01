(ns compiler.frontend.integer
  (:require
   [compiler.frontend.common.id :as id]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.common.error :as err]
   [compiler.frontend.expression :as expr]
   [compiler.middleend.ir :as ir]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))


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
  [[::ir/assign into (::value c)]])


(p/def-op expr/parse-expr unary-minus
  {:precedence 4}
  [_ (token ::lex/minus)
   e expr/parse-expr]
  {::ast/kind ::unary-minus
   ::ast/children [::child]
   ::child e})

(defmethod ast/pretty-print ::unary-minus [n]
  (str "(-" (::child n) ")"))

(defmethod expr/to-ir ::unary-minus [n into]
  (let [tmp (id/make-tmp)
        prev (expr/to-ir (::child n) tmp)]
    (conj prev
          [::ir/assign into [::ir/negate tmp]]))) ; todo: unary minus instead negate


(defn- bin-op-node [op left right]
  {::ast/kind op
   ::ast/children [::left ::right]
   ::left left
   ::right right})

(defn- pretty-print-binop [node op-str]
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print (::right node)) ")"))

(defn- to-ir-bin-op [n res op]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        l-prev (expr/to-ir (::left n) l)
        r-prev (expr/to-ir (::right n) r)]
    (conj (into [] (concat l-prev r-prev))
          [::ir/assign res [op l r]])))


(p/def-op expr/parse-expr plus
  {:precedence 2 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/plus)
   right expr/parse-expr]
  (bin-op-node ::plus left right))

(defmethod ast/pretty-print ::plus [n] (pretty-print-binop n "+"))

(defmethod expr/to-ir ::plus [n res] (to-ir-bin-op n res ::ir/plus))


(p/def-op expr/parse-expr minus
  {:precedence 2 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/minus)
   right expr/parse-expr]
  (bin-op-node ::minus left right))

(defmethod ast/pretty-print ::minus [n] (pretty-print-binop n "-"))

(defmethod expr/to-ir ::minus [n res] (to-ir-bin-op n res ::ir/minus))


(p/def-op expr/parse-expr mul
  {:precedence 3 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/mul)
   right expr/parse-expr]
  (bin-op-node ::mul left right))

(defmethod ast/pretty-print ::mul [n] (pretty-print-binop n "*"))

(defmethod expr/to-ir ::mul [n res] (to-ir-bin-op n res ::ir/mul))


(p/def-op expr/parse-expr div
  {:precedence 3 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/div)
   right expr/parse-expr]
  (bin-op-node ::div left right))

(defmethod ast/pretty-print ::div [n] (pretty-print-binop n "/"))

(defmethod expr/to-ir ::div [n res] (to-ir-bin-op n res ::ir/div))


(p/def-op expr/parse-expr mod
  {:precedence 3 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/mod)
   right expr/parse-expr]
  (bin-op-node ::mod left right))

(defmethod ast/pretty-print ::mod [n] (pretty-print-binop n "%"))

(defmethod expr/to-ir ::mod [n res] (to-ir-bin-op n res ::ir/mod))