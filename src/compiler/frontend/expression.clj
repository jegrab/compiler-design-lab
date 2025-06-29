(ns compiler.frontend.expression
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.math :as math]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.common.error :as err]
   [compiler.frontend.common.id :as id]
   [compiler.frontend.statement :as stmt]
   [compiler.frontend.common.namespace :as name]))

(s/def ::kind keyword?)
(s/def ::parse-expr (s/keys :req [::ast/kind]
                            :opt [::type]))

(defmulti to-ir (fn [expr into] (::ast/kind expr)))

(defmulti typecheck (fn [expr env] (::ast/kind expr)))

(p/def-op-parser parse-expr)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))


(p/def-op parse-expr parantheses
  [_ (token ::lex/left-parentheses)
   e parse-expr
   _ (token ::lex/right-parentheses)]
  e)


(p/defrule stmt/parse-simp ::expr-stmt
  (p/p-let [e parse-expr]
           {::ast/kind ::expr-stmt
            ::ast/children [::expr]
            ::expr e}))

(defmethod ast/pretty-print ::expr-stmt [e]
  (str (ast/pretty-print (::expr e)) ";"))

(defmethod name/resolve-names-stmt ::expr-stmt [e env]
  [(assoc e ::expr (name/resolve-names-expr (::expr e) env))
   env])

(defmethod stmt/to-ir ::expr-stmt [e]
  (let [tmp (id/make-tmp)]
    (to-ir (::expr e) tmp)))

(defmethod stmt/typecheck ::expr-stmt [e env]
  [(assoc e ::expr (typecheck (::expr e) env))
   env])

(defmethod name/check-initialization-stmt ::expr-stmt [e env]
  [(assoc e ::expr (name/check-initialization-expr (::expr e) env))
   env])