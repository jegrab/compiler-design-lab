(ns compiler.frontend.statement
  (:require [compiler.frontend.common.lexer :as lex] 
            [compiler.frontend.common.parser :as p] 
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.expression :as expr]))

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

(defmulti minimal-flow-paths 
  "returns a seq of all minimal control flow paths.
   e.g. for if-then-else both branches
   but for loops only the empty sequence.
   Each path is a vector of statements."
  (fn [stmt] (::ast/kind stmt)))

(defmulti ends-flow ::ast/kind)
(defmethod ends-flow :default [_] false)