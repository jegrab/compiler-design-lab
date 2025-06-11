(ns compiler.frontend.while
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.common.namespace :as name]
            [compiler.frontend.bool :as bool]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.id :as id]
            [compiler.frontend.for :as for]
            [compiler.frontend.block :as block]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule stmt/parse-statement ::while
  [_ (token ::lex/while)
   _ (token ::lex/left-parentheses)
   test expr/parse-expr
   _ (token ::lex/right-parentheses)
   body stmt/parse-statement]
  (for/for-node nil test nil body))