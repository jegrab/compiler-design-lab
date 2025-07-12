(ns compiler.frontend.bool
  (:require [compiler.frontend.common.id :as id]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.parser :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.type :as type]
            [compiler.frontend.expression :as expr]
            [compiler.middleend.oldir :as old-ir]
            [compiler.middleend.ir :as ir]))

(def bool-type (type/simple-type ::bool))
(defmethod type/size-in-bit ::bool [_] 32)

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
  [[::old-ir/assign into (if (::value c) 1 0)]])

(defmethod ast/gen-ir ::boolean-constant [state const]
  (let [value (if (::value const) 1 0)
        const (ir/make-constant 32 value)]
    (ir/add-instruction state (ir/move const (::ir/target state)))))

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
    (conj prev [::old-ir/assign res [::old-ir/not tmp]])))

(defmethod ast/gen-ir ::boolean-negation [state neg]
  (ir/add-instruction (ast/gen-ir state (::child neg))
                      (ir/un-op ::ir/bool-not (::ir/target state) (::ir/target state))))

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
              [[::old-ir/if-false-jmp l label-false]]
              (expr/to-ir (::right a) r)
              [[::old-ir/if-false-jmp r label-false]]
              [[::old-ir/assign res 1]
               [::old-ir/goto label-end]]
              [[::old-ir/target label-false]
               [::old-ir/assign res 0]]
              [[::old-ir/target label-end]]))))

(defmethod ast/gen-ir ::and [state and]
  (let [target (::ir/target state)
        left (ir/make-name 32 "left")
        label-false (id/make-label "false")
        label-true (id/make-label "true")
        label-end (id/make-label "end") 
        
        state (ast/gen-ir (assoc state ::ir/target left) (::left and))
        state (ir/set-cont state (ir/if-then-else left label-true label-false))
        
        state (ir/add-block state label-true)
        state (ast/gen-ir (assoc state ::ir/target target) (::right and))
        state (ir/set-cont state (ir/goto label-end))
        
        state (ir/add-block state label-false) 
        state (ir/add-instruction state (ir/move (ir/make-constant 32 0) target))
        state (ir/set-cont state (ir/goto label-end))
        
        state (ir/add-block state label-end)]
    (assoc state ::ir/target target)))

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
              [[::old-ir/if-true-jmp l label-true]]
              (expr/to-ir (::right a) r)
              [[::old-ir/if-true-jmp r label-true]]
              [[::old-ir/assign res 0]
               [::old-ir/goto label-end]]
              [[::old-ir/target label-true]
               [::old-ir/assign res 1]]
              [[::old-ir/target label-end]]))))

(defmethod ast/gen-ir ::or [state or]
  (let [target (::ir/target state)
        left (ir/make-name 32 "left")
        label-false (id/make-label "false")
        label-true (id/make-label "true")
        label-end (id/make-label "end")

        state (ast/gen-ir (assoc state ::ir/target left) (::left or))
        state (ir/set-cont state (ir/if-then-else left label-true label-false))

        state (ir/add-block state label-false)
        state (ast/gen-ir (assoc state ::ir/target target) (::right or))
        state (ir/set-cont state (ir/goto label-end))

        state (ir/add-block state label-true)
        state (ir/add-instruction state (ir/move (ir/make-constant 32 1) target))
        state (ir/set-cont state (ir/goto label-end))

        state (ir/add-block state label-end)]
    (assoc state ::ir/target target)))

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
              [[::old-ir/if-equal-jmp l r label-eq]
               [::old-ir/assign res 0] ; not equal -> res is false
               [::old-ir/goto label-end]
               [::old-ir/target label-eq]
               [::old-ir/assign res 1] ; equal -> res is true
               [::old-ir/target label-end]]))))

(defmethod ast/gen-ir ::equal [state eq]
  (let [t (::ir/target state)
        l (ir/make-name (type/size-in-bit (::type/type (::left eq))))
        r (ir/make-name (type/size-in-bit (::type/type (::right eq))))
        state (ast/gen-ir (assoc state ::ir/target l) (::left eq))
        state (ast/gen-ir (assoc state ::ir/target r) (::right eq))]
    (ir/add-instruction (assoc state ::ir/target t) (ir/bin-op ::ir/equal? l r t))))


(p/def-op expr/parse-expr not-equal
  {:precedence 8 :associates :left}
  [l expr/parse-expr
   _ (token ::lex/not-equal)
   r expr/parse-expr]
  (bin-op-node ::not-equal l r))

(defmethod ast/pretty-print ::not-equal [e] (pretty-print-binop e "!="))

(defmethod expr/typecheck ::not-equal [e env] (typecheck-bin-op-poylmorphic e env))

(defmethod ast/gen-ir ::not-equal [state eq]
  (let [t (::ir/target state)
        l (ir/make-name (type/size-in-bit (::type/type (::left eq))))
        r (ir/make-name (type/size-in-bit (::type/type (::right eq))))
        state (ast/gen-ir (assoc state ::ir/target l) (::left eq))
        state (ast/gen-ir (assoc state ::ir/target r) (::right eq))]
    (ir/add-instruction (assoc state ::ir/target t) (ir/bin-op ::ir/not-equal? l r t))))