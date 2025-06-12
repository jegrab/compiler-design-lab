(ns compiler.frontend.for
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.variable :as var]
            [compiler.frontend.common.namespace :as name]
            [compiler.frontend.bool :as bool]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.id :as id]
            [compiler.middleend.ir :as ir]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(def ^:dynamic in-loop false)
(def ^:dynamic dyn-label-cont nil)
(def ^:dynamic dyn-label-end nil)

(p/defrule stmt/parse-statement ::break
  [_ (token ::lex/break)
   _ (token ::lex/semicolon)]
  {::ast/kind ::break
   ::ast/children []})

(defmethod ast/pretty-print ::break [_] "break;")
(defmethod name/resolve-names-stmt ::break [break env] [break env])
(defmethod stmt/typecheck ::break [break env] [break env])
(defmethod ast/check-after-parse ::break [break]
  (if (not in-loop)
    (err/add-error break (err/make-semantic-error "break outside of loop"))
    break))
(defmethod stmt/to-ir ::break [_] [[::ir/goto dyn-label-end]])
(defmethod stmt/minimal-flow-paths ::break [break] [[break]])

(p/defrule stmt/parse-statement ::continue
  [_ (token ::lex/continue)
   _ (token ::lex/semicolon)]
  {::ast/kind ::continue
   ::ast/children []})

(defmethod ast/pretty-print ::continue [_] "continue;")
(defmethod name/resolve-names-stmt ::continue [continue env] [continue env])
(defmethod stmt/typecheck ::continue [continue env] [continue env])
(defmethod ast/check-after-parse ::continue [c]
  (if (not in-loop)
    (err/add-error c (err/make-semantic-error "continue outside of loop"))
    c))
(defmethod stmt/to-ir ::continue [_] [[::ir/goto dyn-label-cont]])
(defmethod stmt/minimal-flow-paths ::continue [cont] [[cont]])

(defn for-node [init test step body]
  {::ast/kind ::for
   ::ast/children [::init ::test ::step ::body]
   ::init init
   ::test test
   ::step step
   ::body body})

(p/defrule stmt/parse-statement ::for
  [_ (token ::lex/for)
   _ (token ::lex/left-parentheses)
   init (p/maybe stmt/parse-simp)
   _ (token ::lex/semicolon)
   test expr/parse-expr
   _ (token ::lex/semicolon)
   step (p/maybe stmt/parse-simp)
   _ (token ::lex/right-parentheses)
   body stmt/parse-statement]
  (for-node init test step body))

(defmethod ast/check-after-parse ::for [for] 
  (let [step-is-decl (= ::var/declare (::ast/kind (::step for)))
        for (if step-is-decl
              (err/add-error for (err/make-semantic-error (str "step statement in loop can't be a declaration")))
              for)]
    (binding [in-loop true]
      (assoc for
             ::test (ast/check-after-parse (::test for))
             ::body (ast/check-after-parse (::body for))))))

(defmethod ast/pretty-print ::for [for]
  (str "for (" (if (::init for) (ast/pretty-print (::init for)) "") ";" (ast/pretty-print (::test for)) ";" (if (::step for) (ast/pretty-print (::step for)) "") ")\n{"
       (ast/pretty-print (::body for))
       "}\n"))

(defmethod name/resolve-names-stmt ::for [for env]
  (let [[init new-env] (if (::init for) (name/resolve-names-stmt (::init for) env) [(::init for) env])
        test (name/resolve-names-expr (::test for) new-env)
        [step new-env] (if (::step for) (name/resolve-names-stmt (::step for) new-env) [(::step for) new-env])
        [body _] (name/resolve-names-stmt (::body for) new-env)
        for (assoc for
                   ::init init
                   ::test test
                   ::step step
                   ::body body)]
    [for env]))

(defmethod stmt/typecheck ::for [for env]
  (let [[init new-env] (if (::init for) (stmt/typecheck (::init for) env) [(::init for) env])
        test (expr/typecheck (::test for) new-env)
        [step new-env] (if (::step for) (stmt/typecheck (::step for) new-env) [(::step for) new-env])
        [body _] (stmt/typecheck (::body for) new-env)
        for (assoc for
                   ::test test
                   ::body body)
        for (if (type/equals (::type/type test) bool/bool-type)
              for
              (err/add-error for (err/make-semantic-error (str "type mismatch. Test in loop has type " (::type/type test) " but should have type bool."))))]
    [for env]))

(defmethod stmt/to-ir ::for [for]
  (let [test-tmp (id/make-tmp)
        label-start (id/make-label "start")
        label-cont (id/make-label "cont")
        label-end (id/make-label "end")]
    (binding
     [dyn-label-cont label-cont
      dyn-label-end label-end]
      (into [] (concat
                (if (::init for) (stmt/to-ir (::init for)) [])
                [[::ir/target label-start]]
                (expr/to-ir (::test for) test-tmp)
                [[::ir/if-false-jmp test-tmp label-end]]
                (stmt/to-ir (::body for))
                [[::ir/target label-cont]]
                (if (::step for) (stmt/to-ir (::step for)) [])
                [[::ir/goto label-start]
                 [::ir/target label-end]])))))

(defmethod stmt/minimal-flow-paths ::for [block] [[]])
