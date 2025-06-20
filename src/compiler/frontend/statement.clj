(ns compiler.frontend.statement
  (:require [compiler.frontend.common.lexer :as lex] 
            [compiler.frontend.common.parser :as p] 
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.error :as err]))

(p/defmultiparser parse-statement)
(p/defmultiparser parse-simp)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule parse-statement ::simple (p/p-let [s parse-simp
                                              _ (token ::lex/semicolon)]
                                             s))

(defmulti typecheck (fn [stmt env] (::ast/kind stmt)))

(defmulti to-ir (fn [stmt] (::ast/kind stmt)))

(defmulti returns ::ast/kind)
(defmethod returns :default [_] false)

(defmulti ends-flow ::ast/kind)
(defmethod ends-flow :default [_] false)