(ns compiler.frontend.program 
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt] 
            [compiler.frontend.expression :as expr]
            [compiler.frontend.variable :as var]
            
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
   ::ret-expr ret-expr})

(defmethod ast/pretty-print ::return [ret]
  (str "return " (ast/pretty-print (::ret-expr ret))))

(defmethod ast/semantic-analysis ::return [ret state]
  (let [[ret-expr state] (ast/semantic-analysis (::ret-expr ret) state)
        ret (assoc ret ::ret-expr ret-expr)]
    [(if (not= :int (::expr/type ret-expr))
       (err/add-error ret (err/make-semantic-error (str "type mismatch between return type int and actual type " (::expr/type ret-expr)) ))
       ret)
     state]))

(defmulti is-return ::ast/kind)
(defmethod is-return ::return [_] true)
(defmethod is-return :default [_] false)

(defn build-ast [source-str]
  (let [tokens (lex/lex source-str)
        stmts (p/run program-parser tokens)]
    (if (::p/success stmts)
      (let [analysed (loop [state {}
                            to-do (::p/value stmts)
                            done []
                            has-return false]
                       (if (empty? to-do) 
                         {::code done
                          ::errors (if-not has-return
                                     #{(err/make-semantic-error "missing return statement")}
                                     nil)}
                         (let [[stmt new-state] (ast/semantic-analysis (first to-do) state)]
                           (recur new-state
                                  (rest to-do)
                                  (conj done stmt)
                                  (or has-return (is-return stmt))))))]
        analysed)
      {::code nil
       ::errors #{(err/make-parser-error "unknown fatal parser error")}})))