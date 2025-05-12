(ns compiler.frontend.expression
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]))

(s/def ::kind keyword?)
(s/def ::parse-expr (s/keys :req [::kind]))

(p/def-op-parser parse-expr)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(defn- id-node [name]
  {::ast/kind ::identifier
   ::ast/children []
   ::name name})

(defmethod ast/pretty-print ::identifier [id] :else (str (::name id)))

(p/def-op parse-expr identifier
  [i (token ::lex/identifier)]
  (id-node (::lex/source-string i)))


(p/def-op parse-expr numerical-constant
  [i (token ::lex/numerical-constant)]
  {::ast/kind ::numerical-constant
   ::ast/children [::value]
   ::value (::lex/value i)})

(defmethod ast/pretty-print ::numerical-constant [c] (str (::value c)))

(p/def-op parse-expr parantheses
  [_ (token ::lex/left-parentheses)
   e parse-expr
   _ (token ::lex/right-parentheses)]
  parse-expr)

(defn- bin-op-node [op left right]
  {::ast/kind op
   ::ast/children [::left ::right]
   ::left left
   ::right right})

(defn- un-op-node [op child]
  {::ast/kind op
   ::ast/children [::child]
   ::child child})

(defmethod ast/pretty-print ::call [call]
  (let [arg-strs (mapv ast/pretty-print (::arguments call))]
    (str (ast/pretty-print (::function call))
         "("
         (str/join ", " arg-strs)
         ")")))

(p/def-op parse-expr unary-minus
  {:precedence 4}
  [_ (token ::lex/minus)
   e parse-expr]
  (un-op-node ::negate e))

(defmethod ast/pretty-print ::negate [n]
  (str "(-" (::child n) ")"))

(p/def-op parse-expr plus
  {:precedence 2 :associates :left}
  [left parse-expr
   _ (token ::lex/plus)
   right parse-expr]
  (bin-op-node ::plus left right))

(defn- prett-print-binop [node op-str]
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print(::right node)) ")"))

(defmethod ast/pretty-print ::plus [n]
  (prett-print-binop n "+"))

(p/def-op parse-expr minus
  {:precedence 2 :associates :left}
  [left parse-expr
   _ (token ::lex/minus)
   right parse-expr]
  (bin-op-node ::minus left right))

(defmethod ast/pretty-print ::minus [n]
  (prett-print-binop n "-"))

(p/def-op parse-expr mul
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/mul)
   right parse-expr]
  (bin-op-node ::mul left right))

(defmethod ast/pretty-print ::mul [n]
  (prett-print-binop n "*"))

(p/def-op parse-expr div
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/div)
   right parse-expr]
  (bin-op-node ::div left right))

(defmethod ast/pretty-print ::div [n]
  (prett-print-binop n "/"))

(p/def-op parse-expr mod
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/mod)
   right parse-expr]
  (bin-op-node ::mod left right))

(defmethod ast/pretty-print ::mod [n]
  (prett-print-binop n "%"))