(ns compiler.frontend.bool
  (:require [compiler.frontend.common.id :as id]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.expression :as expr]
            [compiler.middleend.ir :as ir]))

(def bool-type (type/simple-type ::bool))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule type/parse ::bool
  [b (token ::lex/bool)]
  bool-type)

(p/def-op expr/parse-expr true
  [_ (token ::lex/true)]
  {::ast/kind ::boolean-constant
   ::ast/children []
   ::value true})

(p/def-op expr/parse-expr false
  [_ (token ::lex/false)]
  {::ast/kind ::boolean-constant
   ::ast/children []
   ::value false})

(defmethod ast/pretty-print ::boolean-constant [c] (str (::value c)))

(defmethod expr/to-ir ::boolean-constant [c into]
  [[::ir/assign into (if (::value c) 1 0)]])

(defmethod expr/typecheck ::boolean-constant [c _]
  (assoc c ::type/type bool-type))


(p/def-op expr/parse-expr boolean-negation 
  {:precedence 13}
  [_ (token ::lex/log-not)
   e expr/parse-expr]
  {::ast/kind ::boolean-negation
   ::ast/children [::child]
   ::child e})

(defmethod ast/pretty-print ::boolean-negation [n]
  (str "(!" (ast/pretty-print (::child n)) ")"))

(defmethod expr/typecheck ::boolean-negation [n env]
  (let [new-c (expr/typecheck (::child n) env)
        t (::type/type new-c)
        new-n (assoc n
                     ::type/type bool-type
                     ::child new-c)]
    (if (type/equals t bool-type)
      new-n
      (err/add-error new-n
                     (err/make-semantic-error (str "type mismatch: unary negation requires bool but got " t))))))

(defmethod expr/to-ir ::boolean-negation [n into]
  (let [tmp (id/make-tmp)
        prev (expr/to-ir (::child n) tmp)]
    (conj prev [::ir/assign into [::ir/not tmp]])))


