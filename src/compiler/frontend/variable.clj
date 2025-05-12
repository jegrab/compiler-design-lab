(ns compiler.frontend.variable
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.statement :as stmt]))0

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))
                                                    

(p/defrule stmt/parse-statement
  [type (token ::lex/int)
   name (token ::lex/identifier)
   value (p/maybe (p/p-let [_ (token ::lex/assign)
                            e expr/parse-expr]
                           e))
   _ (token ::lex/semicolon)]
  {::ast/kind ::declare
   ::ast/children [::value]
   ::value value
   ::name (::lex/source-string name)})
                                                    
(defmethod ast/pretty-print ::declare [decl]
  (str "int " (::name decl) (if (::value decl) (str " = "(ast/pretty-print (::value decl))) "") ";"))