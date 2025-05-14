(ns compiler.frontend.statement
  (:require [compiler.frontend.common.lexer :as lex] 
            [compiler.frontend.common.parser :as p] 
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.expression :as expr]))

(p/defmultiparser parse-statement)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(defmulti to-ir (fn [stmt] (::ast/kind stmt)))