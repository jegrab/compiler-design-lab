(ns compiler.frontend.program
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.variable :as var]
            [compiler.frontend.integer :as int]
            [compiler.frontend.bool :as bool]
            [compiler.frontend.intasnop :as intasnop]
            [compiler.frontend.ternary :as ternary]
            [compiler.frontend.if :as if]
            [compiler.frontend.intcomp :as intcomp]
            [compiler.frontend.block :as block]
            [compiler.frontend.common.namespace :as name]
            [compiler.middleend.ir :as ir]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule stmt/parse-statement ::return
  [_ (token ::lex/return)
   ret-expr expr/parse-expr
   _ (token ::lex/semicolon)]
  {::ast/kind ::return
   ::ast/children [::ret-expr]
   ::ret-expr ret-expr})

(defmethod ast/pretty-print ::return [ret]
  (str "return " (ast/pretty-print (::ret-expr ret))))

(defmethod name/resolve-names-stmt ::return [ret env]
  [(assoc ret
          ::ret-expr (name/resolve-names-expr (::ret-expr ret) env))
   env])

(defmethod stmt/to-ir ::return [ret]
  (conj
   (expr/to-ir (::ret-expr ret) ::ir/ret-register)
   [::ir/return]))

(defmethod stmt/typecheck ::return [ret env]
  (let [decl-type (env ::ret-type)
        new-expr (expr/typecheck (::ret-expr ret) env)
        actual-type (::type/type new-expr)
        new-ret (assoc ret ::ret-expr new-expr)]
    [(if (type/equals decl-type actual-type)
       new-ret
       (err/add-error new-ret (err/make-semantic-error (str "type mismatch. should return " decl-type " but returns " actual-type))))
     env]))

(defmethod stmt/minimal-flow-paths ::return [ret]
  [[ret]])

(defmulti is-return ::ast/kind)
(defmethod is-return ::return [_] true)
(defmethod is-return :default [_] false)


(def program-parser
  (p/p-let
   [_ (token ::lex/int)
    _ (fn [tok] (and (= (::lex/kind tok) ::lex/identifier)
                     (= (::lex/source-string tok) "main")))
    _ (token ::lex/left-parentheses)
    _ (token ::lex/right-parentheses)
    body block/parse-block
    _ (p/end-of-file)]
   {::ast/kind ::program
    ::ast/children [::body]
    ::body body
    ::ret-type int/int-type}))

(defn take-throughv
  "like take while, but also includes the first element where the predicate does not hold"
  [pred coll]
  (loop [coll coll
         res []]
    (cond
      (empty? coll) res
      (pred (first coll)) (recur (rest coll)
                                 (conj res (first coll)))
      :else (conj res (first coll)))))


(defn build-ast [source-str]
  (let [tokens (lex/lex source-str)
        prog (p/run program-parser tokens)
        ret-type (::ret-type prog)]
    ;(println "source str: " source-str)
    ;(println "tokens: " (map ::lex/kind tokens))
    (cond
      (some err/has-error? tokens)
      {::code nil
       ::errors #{(err/make-parser-error "illegal token detected")}}

      (::p/success prog)
      (let [env (assoc var/default-env ::ret-type int/int-type)
            body (::body (::p/value prog))
            body (ast/check-after-parse body)
            [body env] (name/resolve-names-stmt body env)
            [body env] (stmt/typecheck body env)
            flows (stmt/minimal-flow-paths body)
            all-flows-contain-return (every? #(some is-return %) flows)
            flows-up-to-return (map #(take-throughv (comp not is-return) %) flows)]
        (println "flows-to-return: " (map #(mapv ast/pretty-print %) flows-up-to-return)) 
        {::code body
         ::errors (if-not all-flows-contain-return
                    #{(err/make-semantic-error "missing return statement")}
                    nil)})

      :else
      {::code nil
       ::errors #{(err/make-parser-error "unknown fatal parser error")}})))

(defn to-ir [ast]
  (stmt/to-ir ast))
