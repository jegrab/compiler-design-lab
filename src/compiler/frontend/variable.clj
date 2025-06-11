(ns compiler.frontend.variable
  (:require [compiler.frontend.common.ast :as ast]
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

(def default-env {::names {} ::initialized {} ::declared {} ::types {}})

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

    (and (not ((::initialized env) ((::names env) (::name ident))))
         (not in-l-value))
    (err/add-error ident (err/make-semantic-error (str "accessing uninizialized variable " (::name ident))))

    :else
    (assoc ident ::id ((::names env) (::name ident)))))

(defmethod expr/typecheck ::identifier [id env]
  (assoc id ::type/type (or ((::types env) (::id id)) type/unknown)))

(defmethod expr/to-ir ::identifier [id into]
  [[::ir/assign into (::id id)]])


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
      (assoc-in (assoc-in env [::names name] id)
                [::initialized id] (if (::value decl) true nil))
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
        lv-id (::id l-v)
        expr (name/resolve-names-expr (::expr assign) env)]
    [(assoc assign
            ::l-value l-v
            ::expr expr)
     (assoc-in env [::initialized lv-id] true)]))

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