(ns compiler.frontend.if
  (:require [clojure.set :as set]
            [compiler.frontend.common.id :as id]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.common.namespace :as name]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.statement :as stmt]
            [compiler.middleend.ir :as ir]
            [compiler.frontend.bool :as bool]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule stmt/parse-statement ::if
  [_ (token ::lex/if)
   _ (token ::lex/left-parentheses)
   test expr/parse-expr
   _ (token ::lex/right-parentheses)
   then stmt/parse-statement
   else (p/maybe (p/p-let [_ (token ::lex/else)
                           e stmt/parse-statement]
                          e))]
  {::ast/kind ::if
   ::ast/children [::test ::then ::else]
   ::test test
   ::then then
   ::else else})

(defmethod ast/pretty-print ::if [if]
  (str "if (" (ast/pretty-print (::test if)) ") {\n"
       (ast/pretty-print (::then if))
       "} else {\n"
       (if (::else if) (ast/pretty-print (::else if)) "")
       "}\n"))

(defmethod name/resolve-names-stmt ::if [if env]
  (let [new-test (name/resolve-names-expr (::test if) env)
        [new-then _] (name/resolve-names-stmt (::then if) env)
        [new-else _] (if (::else if) 
                       (name/resolve-names-stmt (::else if) env)
                       [nil nil])]
    [(assoc if
            ::test new-test
            ::then new-then
            ::else new-else)
     env]))

(defmethod stmt/typecheck ::if [if env]
  (let [new-test (expr/typecheck (::test if) env)
        test-type (::type/type new-test)
        [new-then _] (stmt/typecheck (::then if) env)
        [new-else _] (if (::else if)
                       (stmt/typecheck (::else if) env)
                       [nil nil])
        if (assoc if
                  ::test new-test
                  ::then new-then
                  ::else new-else)
        if (if (not (type/equals test-type bool/bool-type))
             (err/add-error if (err/make-semantic-error (str "test of if should be of type " bool/bool-type " but is " test-type)))
             if)]
    [if env]))

(defmethod stmt/to-ir ::if [if]
  (let [test-tmp (id/make-tmp)
        label-else (id/make-label "else")
        label-end (id/make-label "end")]
    (into [] (concat 
              (expr/to-ir (::test if) test-tmp)
              [[::ir/if-false-jmp test-tmp label-else]]
              (stmt/to-ir (::then if))
              [[::ir/goto label-end]]
              [[::ir/target label-else]]
              (if (::else if)
                (stmt/to-ir (::else if))
                [])
              [[::ir/target label-end]]))))

(defmethod stmt/minimal-flow-paths ::if [if]
  (if (::else if)
    (into (stmt/minimal-flow-paths (::then if))
          (stmt/minimal-flow-paths (::else if)))
    (into (stmt/minimal-flow-paths (::then if))
          [[]])))

(defmethod name/check-initialization-stmt ::if [if env]
  (let [[new-then then-env] (name/check-initialization-stmt (::then if) env)
        [new-else else-env] (if (::else env)
                              (name/check-initialization-stmt (::else if) env)
                              [(::else if) env])
        env (assoc env 
                   ::name/initialized (set/intersection (::name/initialized then-env) (::name/initialized else-env)))]
    [(assoc if
            ::then new-then
            ::else new-else)
     env]))