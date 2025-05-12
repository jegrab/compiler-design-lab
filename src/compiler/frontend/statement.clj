(ns compiler.frontend.statement
  (:require [compiler.frontend.common.lexer :as lex] 
            [compiler.frontend.common.parser :as p] 
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.expression :as expr]))

(p/defmultiparser parse-statement)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defmultiparser asnop-parser)
(p/defrule asnop-parser [_ (token ::lex/assign)] ::assign)
(p/defrule asnop-parser [_ (token ::lex/plus-assign)] ::plus-assign)
(p/defrule asnop-parser [_ (token ::lex/minus-assign)] ::minus-assign)
(p/defrule asnop-parser [_ (token ::lex/mul-assign)] ::mul-assign)
(p/defrule asnop-parser [_ (token ::lex/div-assign)] ::div-assign)
(p/defrule asnop-parser [_ (token ::lex/mod-assign)] ::mod-assign)


(defmulti is-l-value ::ast/kind)
(defmethod is-l-value :default [_] false)
(defmethod is-l-value ::expr/identifier [_] true)


(p/defrule parse-statement
  [lv expr/parse-expr
   asnop asnop-parser
   expr expr/parse-expr
   _ (token ::lex/semicolon)]
  {::ast/kind ::asnop
   ::ast/children [::l-value ::expr]
   ::l-value (if (is-l-value lv)
               lv
               (err/add-error lv {::err/phase :parser
                                  ::err/severity :failure
                                  ::err/message "not an l-value"}))
   ::asnop asnop
   ::expr expr})

(defmethod ast/pretty-print ::asnop [asnop]
  (str (ast/pretty-print (::l-value asnop)) 
       (case (::asnop asnop)
         ::assign "="
         ::plus-assign "+="
         ::minus-assign "-="
         ::mul-assign "*="
         ::div-assign "/="
         ::mod-assign "%=") 
       (ast/pretty-print (::expr asnop))
       ";"))

