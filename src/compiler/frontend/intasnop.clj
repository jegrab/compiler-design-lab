(ns compiler.frontend.intasnop
  (:require 
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.expression :as expr]
   [compiler.frontend.statement :as stmt]
   [compiler.frontend.integer :as int]
   [compiler.frontend.variable :as var]))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defmultiparser asnop-parser)
(p/defrule asnop-parser ::plus-assign [_ (token ::lex/plus-assign)] ::int/plus)
(p/defrule asnop-parser ::minus-assign [_ (token ::lex/minus-assign)] ::int/minus)
(p/defrule asnop-parser ::mul-assign [_ (token ::lex/mul-assign)] ::int/mul)
(p/defrule asnop-parser ::div-assign [_ (token ::lex/div-assign)] ::int/div)
(p/defrule asnop-parser ::mod-assign [_ (token ::lex/mod-assign)] ::int/mod)
(p/defrule asnop-parser ::bit-or-assign [_ (token ::lex/bit-or-assign)] ::int/bit-or)
(p/defrule asnop-parser ::bit-and-assign [_ (token ::lex/bit-and-assign)] ::int/bit-and)
(p/defrule asnop-parser ::bit-xor-assign [_ (token ::lex/bit-xor-assign)] ::int/bit-xor)


(p/defrule stmt/parse-simp ::assnop
  [lv expr/parse-expr
   assnop asnop-parser
   expr expr/parse-expr]
  (var/assign-node lv (int/bin-op-node assnop lv expr)))