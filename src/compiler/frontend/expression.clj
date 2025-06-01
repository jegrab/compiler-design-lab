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
   [compiler.middleend.ir :as ir]
   [compiler.frontend.common.namespace :as name]))

(s/def ::kind keyword?)
(s/def ::parse-expr (s/keys :req [::ast/kind]
                            :opt [::type]))

(defmulti to-ir (fn [expr into] (::ast/kind expr)))

(p/def-op-parser parse-expr)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))


(p/def-op parse-expr parantheses
  [_ (token ::lex/left-parentheses)
   e parse-expr
   _ (token ::lex/right-parentheses)]
  e)
