(ns compiler.frontend.while
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.common.namespace :as name]
            [compiler.frontend.bool :as bool]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.id :as id]
            [compiler.middleend.ir :as ir]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule stmt/parse-statement ::while
  [_ (token ::lex/while)
   _ (token ::lex/left-parentheses)
   test expr/parse-expr
   _ (token ::lex/right-parentheses)
   body stmt/parse-statement]
  {::ast/kind ::while
   ::ast/children [::test ::body]
   ::test test
   ::body body})

(defmethod ast/pretty-print ::while [while]
  (str "while (" (ast/pretty-print (::test while)) ")\n{"
       (ast/pretty-print (::body while))
       "}\n"))

(defmethod name/resolve-names-stmt ::while [while env]
  (let [test (name/resolve-names-expr (::test while) env)
        [body _] (name/resolve-names-stmt (::body while) env)
        while (assoc while
                     ::test test
                     ::body body)]
    [while env]))

(defmethod stmt/typecheck ::while [while env]
  (let [test (expr/typecheck (::test while) env)
        [body _] (stmt/typecheck (::body while) env)
        while (assoc while
                     ::test test
                     ::body body)
        while (if (type/equals (::type/type test) bool/bool-type)
                while
                (err/add-error while (err/make-semantic-error (str "type mismatch. Test in while loop has type " (::type/type test) " but should have type bool."))))]
    [while env]))

(defmethod stmt/to-ir ::while [while]
  (let [test-tmp (id/make-tmp)
        label-start (id/make-label "start")
        label-end (id/make-label "end")]
    (into [] (concat
              [[::ir/target label-start]]
              (expr/to-ir (::test while) test-tmp)
              [[::ir/if-false-jmp test-tmp label-end]]
              (stmt/to-ir (::body while))
              [[::ir/goto label-start]
               [::ir/target label-end]]))))

(defmethod stmt/minimal-flow-paths ::while [block] [[]])