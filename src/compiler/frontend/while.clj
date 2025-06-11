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

(def ^:dynamic in-loop false)
(def ^:dynamic dyn-label-start nil)
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
(defmethod stmt/to-ir ::continue [_] [[::ir/goto dyn-label-start]])

(defn while-node [test body]
  {::ast/kind ::while
   ::ast/children [::test ::body]
   ::test test
   ::body body})

(p/defrule stmt/parse-statement ::while
  [_ (token ::lex/while)
   _ (token ::lex/left-parentheses)
   test expr/parse-expr
   _ (token ::lex/right-parentheses)
   body stmt/parse-statement]
  (while-node test body))

(defmethod ast/check-after-parse ::while [while]
  (binding [in-loop true]
    (assoc while 
           ::test (ast/check-after-parse (::test while))
           ::body (ast/check-after-parse (::body while)))))

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
    (binding
     [dyn-label-start label-start
      dyn-label-end label-end]
     (into [] (concat
               [[::ir/target label-start]]
               (expr/to-ir (::test while) test-tmp)
               [[::ir/if-false-jmp test-tmp label-end]]
               (stmt/to-ir (::body while))
               [[::ir/goto label-start]
                [::ir/target label-end]])))))

(defmethod stmt/minimal-flow-paths ::while [block] [[]])