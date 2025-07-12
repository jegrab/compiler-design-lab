(ns compiler.frontend.ternary
  (:require [compiler.frontend.common.id :as id]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.expression :as expr]
            [compiler.middleend.oldir :as old-ir]
            [compiler.middleend.ir :as ir] 
            [compiler.frontend.bool :as bool]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/def-op expr/parse-expr ternary
  {:precedence 2 :associates :right}
  [test expr/parse-expr
   _ (token ::lex/question-mark) 
   then expr/parse-expr
   _ (token ::lex/colon)
   else expr/parse-expr]
  {::ast/kind ::ternary
   ::ast/children [::test ::then ::else]
   ::test test
   ::then then
   ::else else})

(defmethod ast/pretty-print ::ternary [t] 
  (str "(" (ast/pretty-print (::test t)) "?" (ast/pretty-print (::then t)) ":" (ast/pretty-print (::else t)) ")"))

(defmethod expr/typecheck ::ternary [t env] 
  (let [t-test (expr/typecheck (::test t) env)
        t-then (expr/typecheck (::then t) env)
        t-else (expr/typecheck (::else t) env)
        res-type (if (type/equals (::type/type t-then) (::type/type t-else))
                   (type/common (::type/type t-then) (::type/type t-else))
                   type/unknown)
        t (assoc t
                 ::type/type res-type
                 ::test t-test
                 ::then t-then
                 ::else t-else)
        t (if (not (type/equals (::type/type t-test) bool/bool-type))
            (err/add-error t (err/make-semantic-error (str "ternary requires type " bool/bool-type " for test condition but got " (::type/type t-test))))
            t)
        t (if (not (type/equals (::type/type t-then) (::type/type t-else)))
            (err/add-error t (err/make-semantic-error (str "ternary requires both branches to have the same type, but then is " (::type/type t-then) " and else is " (::type/type t-else))))
            t)]
    t))

(defmethod expr/to-ir ::ternary [t res]
  (let [test-tmp (id/make-tmp)
        label-else (id/make-label "else")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::test t) test-tmp)
              [[::old-ir/if-false-jmp test-tmp label-else]]
              (expr/to-ir (::then t) res)
              [[::old-ir/goto label-end]]
              [[::old-ir/target label-else]]
              (expr/to-ir (::else t) res)
              [[::old-ir/target label-end]
               [::old-ir/nop]]))))

(defmethod ast/gen-ir ::ternary [state t]
  (let [test (::test t) 
        target (::ir/target state)
        test-tmp (ir/make-name (type/size-in-bit (::type/type test)) "test")
        label-then (id/make-label "then")
        label-else (id/make-label "else")
        label-end (id/make-label "end")
        state (ast/gen-ir (assoc state ::ir/target test-tmp) test)
        state (ir/set-cont state (ir/if-then-else test-tmp label-then label-else))
        state (ir/add-block state label-then)
        state (ast/gen-ir (assoc state ::ir/target target) (::then t))
        state (ir/set-cont state (ir/goto label-end))
        state (ir/add-block state label-else)
        state (ast/gen-ir (assoc state ::ir/target target) (::else t))
        state (ir/set-cont state (ir/goto label-end))
        state (ir/add-block state label-end)]
    state))