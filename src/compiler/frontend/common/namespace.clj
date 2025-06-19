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

(defmulti check-initialization-expr (fn [ast env] (::ast/kind ast)))
(defmethod check-initialization-expr :default [expr env]
  (loop [expr expr
         cs (::ast/children expr)]

    (if (empty? cs)
      expr
      (do
        (recur
         (assoc expr
                (first cs) (check-initialization-expr ((first cs) expr) env))
         (rest cs))))))

(defmulti check-initialization-stmt (fn [ast env] (::ast/kind ast)))

(defn init-env [env] (assoc env ::defined #{} ::initialized #{}))
(defn define [id env]
  (assoc env ::defined (conj (::defined env) id)))
(defn initialize [id env]
  (assoc env ::initialized (conj (::initialized env) id)))