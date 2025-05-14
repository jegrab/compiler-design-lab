(ns compiler.frontend.program 
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt] 
            [compiler.frontend.expression :as expr]
            [compiler.frontend.variable :as var]
            [compiler.frontend.common.namespace :as name]
            
            [compiler.frontend.common.error :as err]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(def program-parser
  (p/p-let
   [_ (token ::lex/int)
    _ (fn [tok] (and (= (::lex/kind tok) ::lex/identifier)
                     (= (::lex/source-string tok) "main")))
    _ (token ::lex/left-parentheses)
    _ (token ::lex/right-parentheses)
    _ (token ::lex/left-brace)
    stmts (p/many stmt/parse-statement)
    _ (token ::lex/right-brace)]
   stmts))


(p/defrule stmt/parse-statement
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

(defmulti is-return ::ast/kind)
(defmethod is-return ::return [_] true)
(defmethod is-return :default [_] false)

(defn build-ast [source-str]
  (let [tokens (lex/lex source-str)
        stmts (p/run program-parser tokens)]
    (if (::p/success stmts)
      (let [analysed (loop [env var/default-env
                            to-do (::p/value stmts)
                            done []
                            has-return false]
                       (if (empty? to-do) 
                         {::code done
                          ::errors (if-not has-return
                                     #{(err/make-semantic-error "missing return statement")}
                                     nil)}
                         (let [[stmt new-env] (name/resolve-names-stmt (ast/check-after-parse (first to-do)) env)] 
                           (recur new-env
                                  (rest to-do)
                                  (conj done stmt)
                                  (or has-return (is-return stmt))))))]
        analysed)
      {::code nil
       ::errors #{(err/make-parser-error "unknown fatal parser error")}})))