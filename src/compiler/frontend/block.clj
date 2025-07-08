(ns compiler.frontend.block
  (:require [clojure.string :as str]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.common.namespace :as name]
            [compiler.middleend.ir :as ir]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(defn block-node [stmts]
  {::ast/kind ::block
   ::ast/children [::stmts]
   ::stmts stmts})

(def parse-block (p/p-let [_ (token ::lex/left-brace)
                     stmts (p/many stmt/parse-statement)
                     _ (token ::lex/right-brace)] 
                    (block-node stmts)))

(p/defrule stmt/parse-statement ::block parse-block)

(defmethod ast/pretty-print ::block [block]
  (str "{\n" (str/join "\n" (map ast/pretty-print (::stmts block))) "\n}\n"))

(defmethod name/resolve-names-stmt ::block [block env]
  (let [new-stmts (loop [old-stmts (::stmts block)
                         new-stmts []
                         env env]
                    (if (empty? old-stmts)
                      new-stmts
                      (let [[s e] (name/resolve-names-stmt (first old-stmts) env)]
                        (recur (rest old-stmts)
                               (conj new-stmts s)
                               e))))]
    [(assoc block
            ::stmts new-stmts)
     env]))

(defmethod stmt/typecheck ::block [block env]
  (let [new-stmts (loop [old-stmts (::stmts block)
                         new-stmts []
                         env env]
                    (if (empty? old-stmts)
                      new-stmts
                      (let [[s e] (stmt/typecheck (first old-stmts) env)]
                        (recur (rest old-stmts)
                               (conj new-stmts s)
                               e))))]
    [(assoc block
            ::stmts new-stmts)
     env]))

(defmethod ast/gen-ir ::block [state block]
  (reduce ast/gen-ir state (::stmts block)))

(defmethod stmt/to-ir ::block [block]
  (loop [stmts (::stmts block)
         res []]
    (cond (empty? stmts) res
          (stmt/ends-flow (first stmts)) (concat res (stmt/to-ir (first stmts)))
          :else
          (recur
           (rest stmts)
           (into [] (concat res (stmt/to-ir (first stmts))))))))

(defmethod stmt/returns ::block [block]
  (some stmt/returns (::stmts block)))

(defmethod name/check-initialization-stmt ::block [block env]
  (loop [stmts (::stmts block)
         new-stmts []
         env env]
    (if (empty? stmts)
      [(assoc block ::stmts new-stmts) env]
      (let [[new-stmt env] (name/check-initialization-stmt (first stmts) env)]
        (recur
         (rest stmts)
         (conj new-stmts new-stmt)
         env)))))