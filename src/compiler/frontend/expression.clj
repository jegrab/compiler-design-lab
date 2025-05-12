(ns compiler.frontend.expression
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.math :as math]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.common.error :as err]))

(s/def ::kind keyword?)
(s/def ::parse-expr (s/keys :req [::kind]
                            :opt [::type]))

(p/def-op-parser parse-expr)

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(p/def-op parse-expr numerical-constant
  [i (token ::lex/numerical-constant)]
  {::ast/kind ::numerical-constant
   ::ast/children [::value]
   ::num-kind (case (::lex/num-kind i)
                ::lex/hex ::hex
                ::lex/dec ::dec)
   ::value (::lex/value i)})

(defmethod ast/pretty-print ::numerical-constant [c] (str (::value c)))

(defmethod ast/semantic-analysis ::numerical-constant [c state]
  (let [c (assoc c ::type :int)
        c (if (and (= ::dec (::num-kind c))
                   (not (<= 0 (::value c) 2147483648)))
            (err/add-error c (err/make-semantic-error (str "the dec number " (::value c) " does not fit into the range of int.")))
            c)
        c (if (and (= ::hex (::num-kind c))
                   (not (<= 0 (::value c) 0xffffffff)))
            (err/add-error c (err/make-semantic-error (str "the hex number " (::value c) " does not fit into the range of int.")))
            c)]
    [c state]))

(defmethod ast/execute ::numerical-constant [c state]
  (assoc state
         ::res (::value c)))

(p/def-op parse-expr parantheses
  [_ (token ::lex/left-parentheses)
   e parse-expr
   _ (token ::lex/right-parentheses)]
  parse-expr)

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

(defmethod ast/semantic-analysis ::negate [n state]
  (let [[new-c n-s] (ast/semantic-analysis (::child n) state)
        new-neg (assoc n ::type :int ::child new-c)]
    (if (= (::type new-c) :int)
      new-neg
      (err/add-error new-neg (err/make-semantic-error (str "negate only works with int. but was given type " (::type new-c)))))))

(p/def-op parse-expr plus
  {:precedence 2 :associates :left}
  [left parse-expr
   _ (token ::lex/plus)
   right parse-expr]
  (bin-op-node ::plus left right))

(defn- pretty-print-binop [node op-str]
  (str "(" (ast/pretty-print (::left node)) op-str (ast/pretty-print(::right node)) ")"))

(defn- analyse-int-binop [node state op-name]
  (let [[new-left state] (ast/semantic-analysis (::left node) state)
        [new-right state] (ast/semantic-analysis (::right node) state)
        new-node (assoc node
                        ::type :int
                        ::left new-left
                        ::right new-right)]
    [(if (or (not= :int (::type new-left))
             (not= :int (::type new-right)))
       (err/add-error new-node (err/make-semantic-error (str "operator " op-name " expects two integers but got " (::type new-left) " and " (::type new-right))))
       new-node)
     state]))

(defn- exect-bin-op [node state fn]
  (let [sl (ast/execute (::left node) state)
        sr (ast/execute (::right node) sl)]
    (assoc sr
           ::res (fn (::res sl) (::res sr)))))

(defmethod ast/pretty-print ::plus [n] (pretty-print-binop n "+"))
(defmethod ast/semantic-analysis ::plus [n s] (analyse-int-binop n s "+"))
(defmethod ast/execute ::plus [n s] (exect-bin-op n s +))

(p/def-op parse-expr minus
  {:precedence 2 :associates :left}
  [left parse-expr
   _ (token ::lex/minus)
   right parse-expr]
  (bin-op-node ::minus left right))

(defmethod ast/pretty-print ::minus [n] (pretty-print-binop n "-"))
(defmethod ast/semantic-analysis ::minus [n s] (analyse-int-binop n s "-"))
(defmethod ast/execute ::minus [n s] (exect-bin-op n s -))

(p/def-op parse-expr mul
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/mul)
   right parse-expr]
  (bin-op-node ::mul left right))

(defmethod ast/pretty-print ::mul [n] (pretty-print-binop n "*"))
(defmethod ast/semantic-analysis ::mul [n s] (analyse-int-binop n s "*"))
(defmethod ast/execute ::mul [n s] (exect-bin-op n s *))

(p/def-op parse-expr div
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/div)
   right parse-expr]
  (bin-op-node ::div left right))

(defmethod ast/pretty-print ::div [n] (pretty-print-binop n "/"))
(defmethod ast/semantic-analysis ::div [n s] (analyse-int-binop n s "/"))
(defmethod ast/execute ::div [n s] (exect-bin-op n s /))

(p/def-op parse-expr mod
  {:precedence 3 :associates :left}
  [left parse-expr
   _ (token ::lex/mod)
   right parse-expr]
  (bin-op-node ::mod left right))

(defmethod ast/pretty-print ::mod [n] (pretty-print-binop n "%"))
(defmethod ast/semantic-analysis ::mod [n s] (analyse-int-binop n s "%"))
(defmethod ast/execute ::mod [n s] (exect-bin-op n s mod))