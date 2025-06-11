(ns compiler.frontend.intcomp
  (:require [compiler.frontend.common.id :as id]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.expression :as expr]
            [compiler.frontend.bool :as bool]
            [compiler.frontend.integer :as int]
            [compiler.middleend.ir :as ir]))


(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(defn- typecheck-bin-op-int-to-bool [op env]
  (let [left (expr/typecheck (::left op) env)
        right (expr/typecheck (::right op) env)
        ta (::type/type left)
        tb (::type/type right)
        op (assoc op
                  ::type/type bool/bool-type
                  ::left left
                  ::right right)]
    (if (and (type/equals ta tb)
             (type/equals ta int/int-type)
             (type/equals tb int/int-type))
      op
      (err/add-error op (err/make-semantic-error (str "type mismatch between " left " and " right " that should both be int"))))))

(defn- bin-op-node [op left right]
  {::ast/kind op
   ::ast/children [::left ::right]
   ::left left
   ::right right})

(defn- pretty-print-binop [node op-str]
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print (::right node)) ")"))

(p/def-op expr/parse-expr less-then
  {:precedence 9 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/less-then)
   r expr/parse-expr]
  (bin-op-node ::less-then l r))

(defmethod ast/pretty-print ::less-then [a] (pretty-print-binop a "<"))

(defmethod expr/typecheck ::less-then [a env] (typecheck-bin-op-int-to-bool a env))

(defmethod expr/to-ir ::less-then [a res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-geq (id/make-label "geq")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left a) l)
              (expr/to-ir (::right a) r)
              [[::ir/if-greater-or-equal-jmp l r label-geq]
               [::ir/assign res 1] ; not geq => less => true
               [::ir/goto label-end]
               [::ir/target label-geq]
               [::ir/assign res 0] ; geq => not less => false
               [::ir/target label-end]]))))

(p/def-op expr/parse-expr greater-equal
  {:precedence 9 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/greater-then-or-equal)
   r expr/parse-expr]
  (bin-op-node ::geq l r))

(defmethod ast/pretty-print ::geq [a] (pretty-print-binop a ">="))

(defmethod expr/typecheck ::geq [a env] (typecheck-bin-op-int-to-bool a env))

(defmethod expr/to-ir ::geq [a res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-geq (id/make-label "geq")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left a) l)
              (expr/to-ir (::right a) r)
              [[::ir/if-greater-or-equal-jmp l r label-geq]
               [::ir/assign res 0] ; not geq => false
               [::ir/goto label-end]
               [::ir/target label-geq]
               [::ir/assign res 1] ; geq => true
               [::ir/target label-end]]))))


(p/def-op expr/parse-expr greater-then
  {:precedence 9 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/greater-then)
   r expr/parse-expr]
  (bin-op-node ::greater-then l r))

(defmethod ast/pretty-print ::greater-then [a] (pretty-print-binop a ">"))

(defmethod expr/typecheck ::greater-then [a env] (typecheck-bin-op-int-to-bool a env))

(defmethod expr/to-ir ::greater-then [a res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-gt (id/make-label "gt")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left a) l)
              (expr/to-ir (::right a) r)
              [[::ir/if-greater-jmp l r label-gt]
               [::ir/assign res 0] ; not gt => false
               [::ir/goto label-end]
               [::ir/target label-gt]
               [::ir/assign res 1] ; gt => true
               [::ir/target label-end]]))))


(p/def-op expr/parse-expr less-then-or-equal
  {:precedence 9 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/less-then-or-equal)
   r expr/parse-expr]
  (bin-op-node ::leq l r))

(defmethod ast/pretty-print ::leq [a] (pretty-print-binop a "<="))

(defmethod expr/typecheck ::leq [a env] (typecheck-bin-op-int-to-bool a env))

(defmethod expr/to-ir ::leq [a res]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        label-gt (id/make-label "gt")
        label-end (id/make-label "end")]
    (into [] (concat
              (expr/to-ir (::left a) l)
              (expr/to-ir (::right a) r)
              [[::ir/if-greater-jmp l r label-gt]
               [::ir/assign res 1] ; not gt => leq => true
               [::ir/goto label-end]
               [::ir/target label-gt]
               [::ir/assign res 0] ; gt => not leq => true
               [::ir/target label-end]]))))