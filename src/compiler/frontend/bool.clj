(ns compiler.frontend.bool
  (:require [compiler.frontend.common.id :as id]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.expression :as expr]
            [compiler.middleend.ir :as ir]))

(def bool-type (type/simple-type ::bool))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/defrule type/parse ::bool
  [b (token ::lex/bool)]
  bool-type)

(p/def-op expr/parse-expr true
  [_ (token ::lex/true)]
  {::ast/kind ::boolean-constant
   ::ast/children []
   ::value true})

(p/def-op expr/parse-expr false
  [_ (token ::lex/false)]
  {::ast/kind ::boolean-constant
   ::ast/children []
   ::value false})

(defmethod ast/pretty-print ::boolean-constant [c] (str (::value c)))

(defmethod expr/to-ir ::boolean-constant [c into]
  [(ir/move (if (::value c) 1 0) into)])

(defmethod expr/typecheck ::boolean-constant [c _]
  (assoc c ::type/type bool-type))


(p/def-op expr/parse-expr boolean-negation 
  {:precedence 13}
  [_ (token ::lex/log-not)
   e expr/parse-expr]
  {::ast/kind ::boolean-negation
   ::ast/children [::child]
   ::child e})

(defmethod ast/pretty-print ::boolean-negation [n]
  (str "(!" (ast/pretty-print (::child n)) ")"))

(defmethod expr/typecheck ::boolean-negation [n env]
  (let [new-c (expr/typecheck (::child n) env)
        t (::type/type new-c)
        new-n (assoc n
                     ::type/type bool-type
                     ::child new-c)]
    (if (type/equals t bool-type)
      new-n
      (err/add-error new-n
                     (err/make-semantic-error (str "type mismatch: unary negation requires bool but got " t))))))

(defmethod expr/to-ir ::boolean-negation [n res]
  (let [tmp (id/make-tmp)
        prev (expr/to-ir (::child n) tmp)]
    (conj prev [::ir/assign res [::ir/not tmp]])))


(defn bin-op-node [op left right]
  {::ast/kind op
   ::ast/children [::left ::right]
   ::left left
   ::right right})

(defn- pretty-print-binop [node op-str]
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print (::right node)) ")"))

(defn- typecheck-bin-op-all-bool [op env]
  (let [left (expr/typecheck (::left op) env)
        right (expr/typecheck (::right op) env)
        ta (::type/type left)
        tb (::type/type right)
        op (assoc op
                  ::type/type bool-type
                  ::left left
                  ::right right)]
    (if (and (type/equals ta tb)
             (type/equals ta bool-type)
             (type/equals tb bool-type))
      op
      (err/add-error op (err/make-semantic-error (str "type mismatch between " left " and " right " that should both be " bool-type))))))

(p/def-op expr/parse-expr and
  {:precedence 4 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/log-and)
   right expr/parse-expr]
  (bin-op-node ::and left right))

(defmethod ast/pretty-print ::and [a] (pretty-print-binop a "&&"))

(defmethod expr/typecheck ::and [a env] (typecheck-bin-op-all-bool a env))

(defmethod expr/to-ir ::and [a res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-false (id/make-label "false")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left a) l)
              [[::ir/if-false-jmp l label-false]]
              (expr/to-ir (::right a) r)
              [[::ir/if-false-jmp r label-false]]
              [[::ir/assign res 1]
               [::ir/goto label-end]]
              [[::ir/target label-false]
               [::ir/assign res 0]]
              [[::ir/target label-end]]))))

(p/def-op expr/parse-expr or
  {:precedence 3 :associates :left}
  [left expr/parse-expr
   _ (token ::lex/log-or)
   right expr/parse-expr]
  (bin-op-node ::or left right))

(defmethod ast/pretty-print ::or [a] (pretty-print-binop a "||"))

(defmethod expr/typecheck ::or [a env] (typecheck-bin-op-all-bool a env))

(defmethod expr/to-ir ::or [a res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-true (id/make-label "true")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left a) l)
              [[::ir/if-true-jmp l label-true]]
              (expr/to-ir (::right a) r)
              [[::ir/if-true-jmp r label-true]]
              [[::ir/assign res 0]
               [::ir/goto label-end]]
              [[::ir/target label-true]
               [::ir/assign res 1]]
              [[::ir/target label-end]]))))

(defn- typecheck-bin-op-poylmorphic [op env]
  (let [left (expr/typecheck (::left op) env)
        right (expr/typecheck (::right op) env)
        ta (::type/type left)
        tb (::type/type right)
        op (assoc op
                  ::type/type bool-type
                  ::left left
                  ::right right)]
    (if (type/equals ta tb)
      op
      (err/add-error op (err/make-semantic-error (str "type mismatch between " left " and " right " that should be equal"))))))

(p/def-op expr/parse-expr equal
  {:precedence 8 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/equal)
   r expr/parse-expr]
  (bin-op-node ::equal l r))

(defmethod ast/pretty-print ::equal [e] (pretty-print-binop e "=="))

(defmethod expr/typecheck ::equal [e env] (typecheck-bin-op-poylmorphic e env))

(defmethod expr/to-ir ::equal [e res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-eq (id/make-label "eq")
        label-end (id/make-label "end")]
    (into [] (concat 
              (expr/to-ir (::left e) l)
              (expr/to-ir (::right e) r)
              [[::ir/if-equal-jmp l r label-eq]
               [::ir/assign res 0] ; not equal -> res is false
               [::ir/goto label-end]
               [::ir/target label-eq]
               [::ir/assign res 1] ; equal -> res is true
               [::ir/target label-end]]))))


(p/def-op expr/parse-expr not-equal
  {:precedence 8 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/not-equal)
   r expr/parse-expr]
  (bin-op-node ::not-equal l r))

(defmethod ast/pretty-print ::not-equal [e] (pretty-print-binop e "!="))

(defmethod expr/typecheck ::not-equal [e env] (typecheck-bin-op-poylmorphic e env))

(defmethod expr/to-ir ::not-equal [e res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-eq (id/make-label "eq")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left e) l)
              (expr/to-ir (::right e) r)
              [[::ir/if-equal-jmp l r label-eq]
               [::ir/assign res 1] ; not equal -> res is true
               [::ir/goto label-end]
               [::ir/target label-eq]
               [::ir/assign res 0] ; equal -> res is false
               [::ir/target label-end]]))))