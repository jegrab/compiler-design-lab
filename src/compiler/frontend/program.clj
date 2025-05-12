(ns compiler.frontend.program 
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt] 
            [compiler.frontend.expression :as expr]
            [compiler.frontend.variable :as var]
            ))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(def program-parser
  (p/p-let
   [_ (token ::lex/int)
    _ (fn [tok] (and (= (::lex/kind tok) ::lex/identifier)
                     (= (::lex/source-string tok) "main")))
    _ (token ::lex/left-parentheses)
    _ (token ::lex/right-parentheses)
    _ (token ::lex/left-brace)
    stmts (p/many stmt/parse-statement)
    _ (token ::lex/right-brace)]
   {::code stmts}))


(p/defrule stmt/parse-statement
  [_ (token ::lex/return)
   ret-expr expr/parse-expr
   _ (token ::lex/semicolon)]
  {::ast/kind ::return
   ::ret-expr ret-expr})

(defmethod ast/pretty-print ::return [ret]
  (str "return " (ast/pretty-print (::ret-expr ret))))

(defn build-ast [source-str]
  (let [tokens (lex/lex source-str)
        prog (p/run program-parser tokens)]
    (if (::p/value prog)
      (::code (::p/value prog))
      prog)))