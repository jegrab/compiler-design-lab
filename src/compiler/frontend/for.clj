(ns compiler.frontend.for
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.block]
            [compiler.frontend.while]
            [compiler.frontend.block :as block]
            [compiler.frontend.while :as while]
            [compiler.frontend.common.error :as err]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule stmt/parse-statement ::for
  [_ (token ::lex/for)
   _ (token ::lex/left-parentheses)
   init (p/maybe stmt/parse-simp)
   _ (token ::lex/semicolon)
   test expr/parse-expr
   _ (token ::lex/semicolon)
   step (p/maybe stmt/parse-simp)
   _ (token ::lex/right-parentheses)
   body stmt/parse-statement]
  (block/block-node
   [(if init init (block/block-node []))
    (while/while-node test
                      (block/block-node
                       [body
                        (if step step (block/block-node []))]))]))