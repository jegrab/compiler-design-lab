(ns compiler.frontend.variable
  (:require [clojure.set :as set]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.namespace :as name]
            [compiler.frontend.common.id :as id]
            [compiler.frontend.common.type :as type]
            [compiler.middleend.ir :as ir]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

;(defmulti used-vars (fn [node] (::ast/kind node)))
;(defmethod used-vars :default [node]
;  (if (vector? node)
;    (apply set/union (map used-vars node))
;    (apply set/union (map #(used-vars (% node)) (::ast/children node)))))
;(defmethod used-vars ::identifier [id] #{id})


(defn collect-used-vars [ast]
  (filter #(= ::identifier (::ast/kind %))
          (tree-seq (fn [n] (or (::ast/kind n) (vector? n)))
                    (fn [node]
                      (if (vector? node)
                        node
                        (map #(% node) (::ast/children node)))) ast)))

(defn used-vars [ast] (collect-used-vars ast))

(def default-init-env {::initialized #{} ::errors #{}})
(defmulti check-initialization
  "takes a node and an env and returns a new env"
  (fn [node env] (::ast/kind node)))
(defmethod check-initialization :default [node env]
  (assoc env
         ::errors
         (into (::errors env)
               (for [used (used-vars node)
                     :when (not ((::initialized env) (::id used)))]
                 (err/make-semantic-error (str "accessing uninitialized variable " (::name used)))))))

(def default-env {::names {} ::declared {} ::types {}})

(def ^:dynamic in-l-value false)

(defn- id-node [name]
  {::ast/kind ::identifier
   ::ast/children []
   ::name name})

(p/def-op expr/parse-expr identifier
  [i (token ::lex/identifier)]
  (id-node (::lex/source-string i)))

(defmethod ast/pretty-print ::identifier [id] :else (str (::name id)))

(defmethod name/resolve-names-expr ::identifier [ident env]
  (cond
    (not ((::names env) (::name ident)))
    (err/add-error ident (err/make-semantic-error (str "accessing unknown variable " (::name ident))))
    
    :else
    (assoc ident ::id ((::names env) (::name ident)))))

(defmethod expr/typecheck ::identifier [id env]
  (assoc id ::type/type (or ((::types env) (::id id)) type/unknown)))

(defmethod expr/to-ir ::identifier [id into]
  [[::ir/assign into (::id id)]])

(defmethod name/check-initialization-expr ::identifier [id env]
  (if-not ((::name/initialized env) (::id id))
    (err/add-error id (err/make-semantic-error (str "accessing uninitialized variable " (::name id))))
    id))


(p/defrule stmt/parse-simp ::declaration
  [type type/parse
   name (token ::lex/identifier)
   value (p/maybe (p/p-let [_ (token ::lex/assign)
                            e expr/parse-expr]
                           e))]
  {::ast/kind ::declare
   ::ast/children [::value]
   ::type/type type
   ::value value
   ::name (::lex/source-string name)})

(defmethod ast/pretty-print ::declare [decl]
  (str "int " (::name decl) (if (::value decl) (str " = " (ast/pretty-print (::value decl))) "") ";"))

(defmethod name/resolve-names-stmt ::declare [decl env]
  (let [id (id/make-var)
        name (::name decl)
        new-decl (assoc decl
                        ::id id
                        ::value (if (::value decl)
                                  (name/resolve-names-expr (::value decl) env)
                                  nil))]
    [(if ((::declared env) name)
       (err/add-error new-decl (err/make-semantic-error (str "variable " name " is already declared.")))
       new-decl)
     (assoc-in
      (assoc-in env [::names name] id)
      [::declared name] true)]))

(defmethod stmt/to-ir ::declare [decl]
  (if (::value decl)
    (expr/to-ir (::value decl) (::id decl))
    []))

(defmethod stmt/typecheck ::declare [decl env]
  (let [new-val (if (::value decl)
                  (expr/typecheck (::value decl) env)
                  (::value decl))
        type (::type/type decl)
        new-env (assoc-in env [::types (::id decl)] type)
        decl (assoc decl ::value new-val)
        new-decl (if (or (not new-val) (type/equals type (::type/type new-val)))
                   decl
                   (err/add-error decl (err/make-semantic-error (str "type mismatch. declared: " type " actual: " (::type/type new-val)))))]
    [new-decl new-env]))

(defmethod stmt/minimal-flow-paths ::declare [decl]
  [[decl]])

(defmethod check-initialization ::declare [decl env]
  (if (::value decl)
    (let [errors (for [used (used-vars (::value decl))
                       :when (not ((::initialized env) (::id used)))]
                   (err/make-semantic-error (str "accessing uninitialized variable " (::name used))))
          env (assoc env ::errors (into (:errors env) errors))
          env (assoc env
                     ::initialized (conj (::initialized env) (::id decl)))]
      env)
    env))

(defmethod name/check-initialization-stmt ::declare [decl env]
  (let [value (if (::value decl)
                (name/check-initialization-expr (::value decl) env)
                (::value decl))
        env (name/define (::id decl) env)
        env (if value
              (name/initialize (::id decl) env)
              env)]
    [(assoc decl
            ::value value)
     env]))

(defmulti is-l-value ::ast/kind)
(defmethod is-l-value :default [_] false)
(defmethod is-l-value ::identifier [_] true)

(defn assign-node [lv expr]
  {::ast/kind ::assign
   ::ast/children [::expr ::l-value]
   ::l-value (if (is-l-value lv)
               lv
               (err/add-error lv (err/make-parser-error (str (ast/pretty-print lv) " is not an l-value"))))
   ::expr expr})

(p/defrule stmt/parse-simp ::assignment
  [lv expr/parse-expr
   _ (token ::lex/assign)
   expr expr/parse-expr]
  (assign-node lv expr))

(defmethod ast/pretty-print ::assign [assign]
  (str (ast/pretty-print (::l-value assign))
       "="
       (ast/pretty-print (::expr assign))
       ";"))

(defmethod name/resolve-names-stmt ::assign [assign env]
  (let [l-v (binding
             [in-l-value true]
              (name/resolve-names-expr (::l-value assign) env))
        expr (name/resolve-names-expr (::expr assign) env)]
    [(assoc assign
            ::l-value l-v
            ::expr expr)
     env]))

(defmethod stmt/to-ir ::assign [assign]
  (expr/to-ir (::expr assign) (::id (::l-value assign))))

(defmethod stmt/typecheck ::assign [assign env]
  (let [new-l-v (expr/typecheck (::l-value assign) env)
        new-expr (expr/typecheck (::expr assign) env)
        assign (assoc assign
                      ::l-value new-l-v
                      ::expr new-expr)
        new-assign (if (type/equals (::type/type new-l-v) (::type/type new-expr))
                     assign
                     (err/add-error assign (err/make-semantic-error (str "type mismatch. left: " (::type/type new-l-v) " right: " (::type/type new-expr)))))]
    [new-assign env]))

(defmethod stmt/minimal-flow-paths ::assign [assign]
  [[assign]])

(defmethod check-initialization ::assign [assign env]
  (let [errors (for [used (used-vars (::expr assign))
                     :when (not ((::initialized env) (::id used)))]
                 (err/make-semantic-error (str "accessing uninitialized variable " (::name used)))) 
        env (assoc env ::errors (into (::errors env) errors))
        env (assoc env
                   ::initialized (conj (::initialized env) (::id (::l-value assign))))]
    env))

(defmethod name/check-initialization-stmt ::assign [assign env]
  (let [expr (name/check-initialization-expr (::expr assign) env)
        env (name/initialize (::id (::l-value assign)) env)]
    [(assoc assign
            ::expr expr)
     env]))


(defn check-init-in-flow [flow]
  (loop [flow flow
         env default-init-env]
    (if (empty? flow)
      env
      (recur (rest flow)
             (let [x (check-initialization (first flow) env)]
               x)))))

(defn check-init-in-all-flows [flows] 
  (set (apply concat (map (comp ::errors check-init-in-flow) flows))))