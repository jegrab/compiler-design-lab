(ns compiler.frontend.variable
  (:require [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.statement :as stmt]
            [compiler.frontend.common.error :as err])) 0

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(defn- id-node [name]
  {::ast/kind ::identifier
   ::ast/children []
   ::name name})

(p/def-op expr/parse-expr identifier
  [i (token ::lex/identifier)]
  (id-node (::lex/source-string i)))

(defmethod ast/pretty-print ::identifier [id] :else (str (::name id)))

(defmethod ast/semantic-analysis ::identifier [id state]
  (let [var-id ((::name-env state) (::name id))
        var-data ((::env state) var-id)]
    [(cond
       (or (not var-id) (not var-data))
       (assoc (err/add-error id (err/make-semantic-error (str "accessing unknown variable " (::name id))))
              ::expr/type :unknown
              ::id nil)

       (and (not (::initialized var-data))
            (not (::in-assignment state)))
       (assoc (err/add-error id (err/make-semantic-error (str "accessing variable " (::name id) " before initializing it.")))
              ::expr/type (::type var-data)
              ::id var-id)

       :else
       (assoc id
              ::expr/type (::type var-data)
              ::id var-id))
     state]))

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
   ::type :int
   ::name (::lex/source-string name)})

(defmethod ast/pretty-print ::declare [decl]
  (str "int " (::name decl) (if (::value decl) (str " = " (ast/pretty-print (::value decl))) "") ";"))

(defmethod ast/semantic-analysis ::declare [decl state]
  (let [[new-val state-after-val] (if (::value decl)
                                   (ast/semantic-analysis (::value decl) state)
                                   [nil state])
        initialized (if (::value decl) true false)
        id (java.util.UUID/randomUUID)
        new-state (assoc state-after-val
                          ::name-env (assoc (::name-env state-after-val) (::name decl) id)
                          ::env (assoc (::env state-after-val)
                                       id {::type (::type decl)
                                           ::name (::name decl)
                                           ::initialized initialized}))
        new-decl (assoc decl ::value new-val)]
    [(if (and new-val (not= (::type decl) (::expr/type new-val)))
       (err/add-error new-decl (err/make-semantic-error (str "type mismatch. Declared " (::type decl) " but given " (::expr/type new-val))))
       new-decl) 
     new-state]))


(p/defmultiparser asnop-parser)
(p/defrule asnop-parser [_ (token ::lex/assign)] ::assign)
(p/defrule asnop-parser [_ (token ::lex/plus-assign)] ::plus-assign)
(p/defrule asnop-parser [_ (token ::lex/minus-assign)] ::minus-assign)
(p/defrule asnop-parser [_ (token ::lex/mul-assign)] ::mul-assign)
(p/defrule asnop-parser [_ (token ::lex/div-assign)] ::div-assign)
(p/defrule asnop-parser [_ (token ::lex/mod-assign)] ::mod-assign)


(defmulti is-l-value ::ast/kind)
(defmethod is-l-value :default [_] false)
(defmethod is-l-value ::identifier [_] true)


(p/defrule stmt/parse-statement
  [lv expr/parse-expr
   asnop asnop-parser
   expr expr/parse-expr
   _ (token ::lex/semicolon)]
  {::ast/kind ::asnop
   ::ast/children [::l-value ::expr]
   ::l-value (if (is-l-value lv)
               lv
               (err/add-error lv (err/make-parser-error (str (ast/pretty-print lv) " is not an l-value"))))
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

(defn- check-type-match [asnop]
  (if (= (::expr/type (::l-value asnop))
         (::expr/type (::expr asnop)))
    asnop
    (err/add-error asnop (err/make-semantic-error (str "type mismatch: assigning '" (::exptype (::expr asnop)) " to " (::expr/type (::l-value asnop)))))))

(defn- check-if-int-op [asnop]
  (if (and (#{::plus-assign ::minus-assign ::mul-assign ::div-assign ::mod-assign} (::asnop asnop))
           (not (= :int (::expr/type (::expr asnop)))))
    (err/add-error asnop (err/make-semantic-error (str "type mismatch: operator " (::asnop asnop) " works only on type int. but has type " (::type (::expr asnop)))))
    asnop))

(defmethod ast/semantic-analysis ::asnop [asnop state]
  (let [[new-e state-after-e] (ast/semantic-analysis (::expr asnop) state)
        [new-l state-after-l] (ast/semantic-analysis (::l-value asnop) (assoc state-after-e ::in-assignment true))
        id (::name-env state)
        state-with-initialzed (assoc-in (assoc state-after-l ::in-assignment false) [::env id ::initialized] true)
        new-asnop (assoc asnop
                         ::l-value new-l
                         ::expr new-e)]
    [(-> new-asnop
         check-type-match
         check-if-int-op)
     state-with-initialzed]))

