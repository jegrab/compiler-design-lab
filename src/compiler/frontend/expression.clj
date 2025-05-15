(ns compiler.frontend.expression
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.math :as math]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.common.error :as err]
   [compiler.frontend.common.id :as id]
   [compiler.middleend.ir :as ir]
   [compiler.frontend.common.namespace :as name]))

(s/def ::kind keyword?)
(s/def ::parse-expr (s/keys :req [::ast/kind]
                            :opt [::type]))

(defmulti to-ir (fn [expr into] (::ast/kind expr)))

(p/def-op-parser parse-expr)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/def-op parse-expr numerical-constant
  [i (token ::lex/numerical-constant)]
  {::ast/kind ::numerical-constant
   ::ast/children []
   ::num-kind (case (::lex/num-kind i)
                ::lex/hex ::hex
                ::lex/dec ::dec)
   ::value (::lex/value i)})

(defmethod ast/pretty-print ::numerical-constant [c] (str (::value c)))

(defmethod ast/check-after-parse ::numerical-constant [c]
  (cond
    (and (= ::dec (::num-kind c)) (not (<= 0 (::value c) 2147483648)))
    (err/add-error c (err/make-semantic-error (str "the dec number " (::value c) " does not fit into the range of int.")))
    
    (and (= ::hex (::num-kind c)) (not (<= 0 (::value c) 0xffffffff)))
    (err/add-error c (err/make-semantic-error (str "the hex number " (::value c) " does not fit into the range of int.")))
    
    :else c))

(defmethod ast/execute ::numerical-constant [c state]
  (assoc state
         ::res (::value c)))

(defmethod to-ir ::numerical-constant [c into]
  [[::ir/assign into (::value c)]])

(p/def-op parse-expr parantheses
  [_ (token ::lex/left-parentheses)
   e parse-expr
   _ (token ::lex/right-parentheses)]
  e)

(defn- bin-op-node [op left right]
  {::ast/kind op
   ::ast/children [::left ::right]
   ::left left
   ::right right})

(defn- un-op-node [op child]
  {::ast/kind op
   ::ast/children [::child]
   ::child child})

(p/def-op parse-expr unary-minus
  {:precedence 4}
  [_ (token ::lex/minus)
   e parse-expr]
  (un-op-node ::negate e))

(defmethod ast/pretty-print ::negate [n]
  (str "(-" (::child n) ")"))

(defmethod ast/execute ::negate [c state]
  (assoc state
         ::res (- (::res (ast/execute c state)))))

(defmethod to-ir ::negate [n into]
  (let [tmp (id/make-tmp)
        prev (to-ir (::child n) tmp)]
    (conj prev
          [::ir/assign into [::ir/negate tmp]])))

(p/def-op parse-expr plus
  {:precedence 2 :associates :left}
  [left parse-expr
   _ (token ::lex/plus)
   right parse-expr]
  (bin-op-node ::plus left right))

(defn- pretty-print-binop [node op-str]
  ;#break
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print (::right node)) ")"))

(defn- exect-bin-op [node state fn]
  (let [sl (ast/execute (::left node) state)
        sr (ast/execute (::right node) sl)]
    (assoc sr
           ::res (fn (::res sl) (::res sr)))))

(defmethod ast/pretty-print ::plus [n] (pretty-print-binop n "+"))
(defmethod ast/execute ::plus [n s] (exect-bin-op n s +))

(defn- to-ir-bin-op [n res op]
  (let [l (id/make-tmp)
        r (id/make-tmp)
        l-prev (to-ir (::left n) l)
        r-prev (to-ir (::right n) r)]
    (conj (into [] (concat l-prev r-prev))
          [::ir/assign res [op l r]])))

(defmethod to-ir ::plus [n res] (to-ir-bin-op n res ::ir/plus))

(p/def-op parse-expr minus
  {:precedence 2 :associates :left}
  [left parse-expr
   _ (token ::lex/minus)
   right parse-expr]
  (bin-op-node ::minus left right))

(defmethod ast/pretty-print ::minus [n] (pretty-print-binop n "-"))
(defmethod ast/execute ::minus [n s] (exect-bin-op n s -))
(defmethod to-ir ::minus [n res] (to-ir-bin-op n res ::ir/minus))

(p/def-op parse-expr mul
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/mul)
   right parse-expr]
  (bin-op-node ::mul left right))

(defmethod ast/pretty-print ::mul [n] (pretty-print-binop n "*"))
(defmethod ast/execute ::mul [n s] (exect-bin-op n s *))
(defmethod to-ir ::mul [n res] (to-ir-bin-op n res ::ir/mul))

(p/def-op parse-expr div
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/div)
   right parse-expr]
  (bin-op-node ::div left right))

(defmethod ast/pretty-print ::div [n] (pretty-print-binop n "/"))
(defmethod ast/execute ::div [n s] (exect-bin-op n s /))
(defmethod to-ir ::div [n res] (to-ir-bin-op n res ::ir/div))

(p/def-op parse-expr mod
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/mod)
   right parse-expr]
  (bin-op-node ::mod left right))

(defmethod ast/pretty-print ::mod [n] (pretty-print-binop n "%"))
(defmethod ast/execute ::mod [n s] (exect-bin-op n s mod))
(defmethod to-ir ::mod [n res] (to-ir-bin-op n res ::ir/mod))