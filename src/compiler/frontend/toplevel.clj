(ns compiler.frontend.toplevel
  (:require [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.ast :as ast]))

(p/defmultiparser toplevel)

(defmulti collect-names (fn [node env] (::ast/kind node)))
(defmulti to-ir ::ast/kind)