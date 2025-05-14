(ns compiler.frontend.common.namespace
  (:require [compiler.frontend.common.ast :as ast]))

(defmulti  resolve-names-expr
  (fn [ast env] (::ast/kind ast)))

(defmethod resolve-names-expr :default [ast env]
  (loop [ast ast
         cs (::ast/children ast)]
    
    (if (empty? cs)
      ast
      (do
        (recur
         (assoc ast
                (first cs) (resolve-names-expr ((first cs) ast) env))
         (rest cs))))))


(defmulti  resolve-names-stmt
  (fn [ast env] (::ast/kind ast)))