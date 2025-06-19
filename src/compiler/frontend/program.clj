(ns compiler.frontend.program
  (:require [clojure.set :as set]
            [compiler.frontend.common.ast :as ast]
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
            [compiler.frontend.while :as while]
            [compiler.frontend.for :as for]
            [compiler.frontend.toplevel :as top]
            [compiler.frontend.function :as fun]
            [compiler.frontend.block :as block]
            [compiler.frontend.common.namespace :as name]
            [compiler.middleend.ir :as ir]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(def program-parser
  (p/p-let
   [decls (p/many top/toplevel)
    _ (p/end-of-file)]
   {::ast/kind ::program
    ::ast/children [::decls]
    ::decls decls}))

(defn thread-env-through [mapper decls env]
  (loop [decls decls
         new-decls []
         env env]
    (if (empty? decls)
      [new-decls env] 
      (let [[d e] (mapper (first decls) env)]
        (recur (rest decls)
               (conj new-decls d)
               e)))))

(defn >>> [decls env mappers] 
  (if (empty? mappers)
    [decls env]
    (let [[d e] (thread-env-through (first mappers) decls env)]
      (recur d e (rest mappers)))))


(defn frontend [source-str]
  (let [tokens (lex/lex source-str)
        prog (p/run program-parser tokens)]
    (cond
      (some err/has-error? tokens)
      {::decls nil
       ::errors #{(err/make-parser-error "illegal token detected")}}

      (::p/success prog)
      (let [env (name/init-env (fun/setup-env (assoc var/default-env ::ret-type int/int-type)))
            decls (::decls (::p/value prog))
            decls (map ast/check-after-parse decls)
            [decls env] (>>> decls env
                             [top/collect-names
                              name/resolve-names-stmt
                              stmt/typecheck
                              name/check-initialization-stmt])
            main (first (filter fun/is-main decls)) 
            errs (apply clojure.set/union (map ast/collect-errors decls))
            errs (if-not main (conj errs (err/make-semantic-error "missing main function"))
                         errs)]
        {::decls decls
         ::main-id (::fun/id main)
         ::errors errs})

      :else
      {::decls nil
       ::errors #{(err/make-parser-error "unknown fatal parser error")}})))

(defn to-ir [decls]
  (into [] (map top/to-ir decls)))

;only support call as expression statement
(defmethod ast/check-after-parse ::expr/expr-stmt [e]
  (if (not= (::ast/kind (::expr/expr e)) ::fun/call)
    (err/add-error e
                   (err/make-parser-error "only function calls allowed as expression statement"))
    e))