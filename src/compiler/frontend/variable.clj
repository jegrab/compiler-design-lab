(ns compiler.frontend.variable
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.namespace :as name]
            [compiler.frontend.common.id :as id]
            [compiler.middleend.ir :as ir]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(def default-env {::env {} ::initialized {}})

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

(defmethod expr/to-ir ::identifier [id into]
  [[::ir/assign into (::id id)]])

(p/defrule stmt/parse-statement
  [type (token ::lex/int)
   name (token ::lex/identifier)
   value (p/maybe (p/p-let [_ (token ::lex/assign)
                            e expr/parse-expr]
                           e))
   _ (token ::lex/semicolon)]
  {::ast/kind ::declare
   ::ast/children [::value]
   ::value value
   ::name (::lex/source-string name)})

(defmethod ast/pretty-print ::declare [decl]
  (str "int " (::name decl) (if (::value decl) (str " = " (ast/pretty-print (::value decl))) "") ";"))

(defmethod name/resolve-names-stmt ::declare [decl env]
  (let [id (id/make-var)
        name (::name decl)]
    [(assoc decl
            ::id id
            ::value (if (::value decl)
                      (name/resolve-names-expr (::value decl) env)
                      nil))
     (assoc-in (assoc-in env [::names name] id)
               [::initialized id] (if (::value decl) true nil))]))

(defmethod stmt/to-ir ::declare [decl]
  (if (::value decl)
    (expr/to-ir (::value decl) (::id decl))
    []))

(p/defmultiparser asnop-parser)
(p/defrule asnop-parser [_ (token ::lex/assign)] ::assign)
(p/defrule asnop-parser [_ (token ::lex/plus-assign)] ::plus-assign)
(p/defrule asnop-parser [_ (token ::lex/minus-assign)] ::minus-assign)
(p/defrule asnop-parser [_ (token ::lex/mul-assign)] ::mul-assign)
(p/defrule asnop-parser [_ (token ::lex/div-assign)] ::div-assign)
(p/defrule asnop-parser [_ (token ::lex/mod-assign)] ::mod-assign)


(defmulti is-l-value ::ast/kind)
(defmethod is-l-value :default [_] false)
(defmethod is-l-value ::identifier [_] true)


(p/defrule stmt/parse-statement
  [lv expr/parse-expr
   asnop asnop-parser
   expr expr/parse-expr
   _ (token ::lex/semicolon)]
  {::ast/kind ::asnop
   ::ast/children [::expr ::l-value]
   ::l-value (if (is-l-value lv)
               lv
               (err/add-error lv (err/make-parser-error (str (ast/pretty-print lv) " is not an l-value"))))
   ::asnop asnop
   ::expr expr})

(defmethod ast/pretty-print ::asnop [asnop]
  (str (ast/pretty-print (::l-value asnop))
       (case (::asnop asnop)
         ::assign "="
         ::plus-assign "+="
         ::minus-assign "-="
         ::mul-assign "*="
         ::div-assign "/="
         ::mod-assign "%=")
       (ast/pretty-print (::expr asnop))
       ";"))

(defmethod name/resolve-names-stmt ::asnop [asnop env]
  (let [l-v (binding
             [in-l-value true]
             (name/resolve-names-expr (::l-value asnop) env))
        lv-id (::id l-v)
        expr (name/resolve-names-expr (::expr asnop) env)]
    [(assoc asnop
            ::l-value l-v
            ::expr expr)
     (assoc-in env [::initialized lv-id] true)]))

(defmethod stmt/to-ir ::asnop [asnop]
  (expr/to-ir (::expr asnop) (::id (::l-value asnop))))