(ns compiler.frontend.common.parser
  (:require [clojure.spec.alpha :as s]))

(s/def ::input seq?)
(s/def ::remaining ::input)
(s/def ::expected (s/and coll? ::combinator))
(s/def ::success boolean?)

(s/def ::success-result (s/keys :req [::value]))
(s/def ::failure-result (s/keys :req [::expected]))
(s/def ::result (s/and
                 (s/keys :req [::remaining ::success])
                 (s/or ::success-result ::failure-result)))

(s/def ::parse-fn (s/fspec
                   :args (s/cat :input ::input)
                   :ret ::result))

(s/def ::combinator (s/keys :req [::parse-fn]
                            :opt [::name]))

(defprotocol Parser
  (run [parser input] "runs the parsen on the input and returns a ::result")
  (map-success [parser mapper] "returns an equivalent parser, but with the successful result value applied to mapper."))

(defn- map-result [mapper result]
  (if (::success result)
    (update result ::value mapper)
    result))

(defrecord SimpleParser [parse-fn]
  Parser
  (run [this input] ((:parse-fn this) input))
  (map-success [this mapper]
    (update this :parse-fn (fn [p-fn] (comp #(map-result mapper %) p-fn)))))

(defn end-of-file []
  (->SimpleParser (fn [input]
                    (if (empty? input)
                      {::remaining input
                       ::success true
                       ::value nil}
                      {::remaining input
                       ::success false
                       ::expected ["end of file"]}))))

(defn- predicate-parser [predicate expected]
  (->SimpleParser (fn [input]
                    (if (predicate (first input))
                      {::remaining (rest input)
                       ::success true
                       ::value (first input)}
                      {::remaining input
                       ::success false
                       ::expected expected}))))

(extend-type clojure.lang.IFn
  Parser
  (run [this input]
    (if (this (first input))
      {::remaining (rest input)
       ::success true
       ::value (first input)}
      {::remaining input
       ::success false
       ::expected [this]}))
  (map-success [this mapper]
    (map-success (predicate-parser this [this]) mapper)))

(defn many
  "tries to apply parser as many times as possible).
   zero times is also success.
   returns the results in a vector.
   never fails."
  [parser]
  (->SimpleParser
   (fn [input]
     (loop [input input
            res []]
       (let [p-res (run parser input)]
         (if (::success p-res)
           (recur (::remaining p-res)
                  (conj res (::value p-res)))
           {::remaining (::remaining p-res)
            ::success true
            ::value res}))))))

(defn maybe 
  "tries to apply the parser.
   if successfull, returns its result.
   if not, returns success with nil as ::value."
  [parser]
  (->SimpleParser 
   (fn [input]
     (let [r (run parser input)]
       (if (::success r) r
           {::value nil ::success true ::remaining input})))))

(defn parse-seq
  "takes a collection of pairs [key parser] and returns a parser that runs all parsers in order and returns a map with each result mapped to the specified key."
  ([parsers & {:keys [expected success-options]}]
   (let [expected (or expected [(map second parsers)])
         success-options (or success-options {})]
     (->SimpleParser (fn [initial-input]
                       (loop [parsers parsers
                              input initial-input
                              val {}]
                         (if (empty? parsers)
                           (merge
                            success-options
                            {::remaining input
                             ::success true
                             ::value val})
                           (let [[key parser] (first parsers)
                                 res (run parser input)]
                             (if (::success res)
                               (recur (rest parsers)
                                      (::remaining res)
                                      (assoc val key (::value res)))
                               {::remaining initial-input
                                ::success false
                                ::expected expected})))))))))

(defn- make-seq-input
  "takes a definition for the defparser macro (a collection [key1 parser1 key2 parser2 ...])
   where 'parser' can be a predicate or a symbol.
   and return as collection of pairs [key parser]
   if parser is a symbol, it is kept
   else it is treated as a predicate and wrapped in predicate-parser."
  [definition]
  (loop [defs definition
         res []]
    (if
     (empty? defs)
      res
      (let [[name parser & rest] defs]
        (if-not (symbol? name)
          (throw (Exception. "result name has to be a symbol."))
          (recur rest (conj res [`(quote ~name) parser])))))))

(defn- make-let-bindings [definitions bindings-name]
  (loop [defs definitions
         res []]
    (if (empty? defs)
      res
      (let [[name parser & rest] defs]
        (recur rest
               (conj res
                     name
                     `(~bindings-name (quote ~name))))))))

(defmacro p-let
  {:clj-kondo/lint-as 'clojure.core/let}
  [parser-definition result-expr]
  (let [b-name (gensym "bind")
        seq-input (make-seq-input parser-definition)
        let-bindings (make-let-bindings parser-definition b-name)]
    `(map-success
      (parse-seq ~seq-input)
      (fn [~b-name]
        (let ~let-bindings
          ~result-expr)))))



(defrecord Multiparser [parser-rules name]
  Parser
  (run [this input]
    (let [runs (map (fn [rule] (run rule input)) @parser-rules)
          successful (filter ::success runs)
          num-sucesses (count successful)]
      (cond
        (> num-sucesses 1) (throw (Exception. (str "grammar rules for " name " are nondeterministic.")))
        (< num-sucesses 1) {::remaining input
                            ::success false
                            ::expected [name]} ;(apply concat (map ::expected runs))}
        (= num-sucesses 1) (first successful))))
  (map-success [this mapper]
    (map-success (->SimpleParser (fn [input] (run this input))) mapper)))

(defn add-rule-to-multi [multiparser rule]
  (update multiparser :parser-rules #(conj % rule)))

(defmacro defmultiparser
  {:clj-kondo/lint-as 'declare}
  [name]
  `(def ~name (->Multiparser (atom []) (quote ~name))))

(defmacro defrule [name & stuff]
  (let [options (if (map? (first stuff)) (first stuff) {})
        stuff (if (map? (first stuff)) (rest stuff) stuff)
        by-other (not (vector? (first stuff)))
        defining-parser (if by-other (first stuff) nil)
        defining-rules (if-not by-other (first stuff) nil)
        stuff (if-not by-other (rest stuff) nil)
        res-expr (if-not by-other (first stuff) nil)]
    (when-not (empty? options) (throw (Exception. "options currently not supported.")))
    (if by-other
      `(swap! (:parser-rules ~name) #(conj % ~defining-parser))
      `(swap! (:parser-rules ~name) #(conj % (p-let ~defining-rules ~res-expr))))))


(defn- try-all-children [parsers input]
  (let [runs (map (fn [parser] (run parser input)) parsers)
        successful (filter ::success runs)
        num-sucesses (count successful)]
    (cond
      (> num-sucesses 1) (throw (Exception. (str "grammar rules" " are nondeterministic.")))
      (< num-sucesses 1) {::remaining input
                          ::success false
                          ::expected (apply concat (map ::expected runs))}
      (= num-sucesses 1) (first successful))))

(defn- exit-parsers [pratt] (map second (filter #(= ::exit (::kind (second %))) @(:parsers pratt))))
(defn- leaf-parsers [pratt] (map second (filter #(= ::leaf (::kind (second %))) @(:parsers pratt))))
(defn- prefix-parsers [pratt] (map second (filter #(= ::prefix (::kind (second %))) @(:parsers pratt))))
(defn- postfix-parsers [pratt] (map second (filter #(= ::postfix (::kind (second %))) @(:parsers pratt))))
(defn- infix-parsers [pratt] (map second (filter #(= ::infix (::kind (second %))) @(:parsers pratt))))

(defn- run-pratt [pratt initial-input min-bp]
  (let [exit-parsers (exit-parsers pratt)
        leaf-parsers (leaf-parsers pratt)
        prefix-parsers (prefix-parsers pratt)
        postfix-parsers (postfix-parsers pratt)
        infix-parsers (infix-parsers pratt)
        
        leaf-res (try-all-children leaf-parsers initial-input)
        prefix-res (try-all-children prefix-parsers initial-input)]
    (when (and (::success leaf-res) (::success prefix-res)) (throw (Exception. "grammar rules are nondeterministic")))
    (loop [lhs (cond (::success leaf-res) leaf-res

                     (::success prefix-res)
                     (let [rhs (run-pratt pratt (::remaining prefix-res) (::right-bp prefix-res))]
                       (if (::success rhs)
                         {::value ((::result-fn prefix-res) (::value prefix-res) (::value rhs))
                          ::success true
                          ::remaining (::remaining rhs)} 
                         nil))

                     :else nil)
           input (::remaining lhs)]
      (let [exit-res (try-all-children exit-parsers input)
            postfix-res (try-all-children postfix-parsers input)
            infix-res (try-all-children infix-parsers input)
            count-successful (count (filter ::success [exit-res postfix-res infix-res]))]
        (when (< 1 count-successful) (throw (Exception. "grammar rules are nondeterministic")))
        (cond
          (not lhs) {::remaining initial-input ::expected [(:name pratt) ::success false]}
          (= 0 count-successful) lhs
          (::success exit-res) lhs

          (::success postfix-res)
          (let [l-bp (::left-bp postfix-res)]
            (if (< l-bp min-bp)
              lhs
              (recur
               {::value ((::result-fn postfix-res) (::value postfix-res) (::value lhs))
                ::success true
                ::remaining (::remaining postfix-res)}
               (::remaining postfix-res))))
          
          (::success infix-res)
          (let [l-bp (::left-bp infix-res)
                r-bp (::right-bp infix-res)]
            (if (< l-bp min-bp)
              lhs
              (let [rhs (run-pratt pratt (::remaining infix-res) r-bp)]
                (if (::success rhs)
                  (recur
                   {::value ((::result-fn infix-res) (::value infix-res) (::value lhs) (::value rhs))
                    ::success true
                    ::remaining (::remaining rhs)} 
                   (::remaining rhs))
                  {::remaining initial-input ::expected [(:name pratt)] ::success false})))))))))

; child parsers are atom of vector of records that represent a parser .
; they also have keys  ::left-bp ::right-bp ::result-fn that are added by the def-op macro
; where result-fn has number-of-rec-calls many parameters and is called with their results.

; exit-parsers ; don't have a recursive call. if one matches, the pratt parser stops and before it.
; leaf-parsers ; neither have recursive calls at start nor end 
; infix-parsers ; have recursive calls at start and end
; prefix-parsers ; have recursive calls at end
; postfix-parsers ; have recursive calls at start

; todo: currently, the outer recursive calls have to be directly in the rule definition to be detected.
; todo: currently, there can't be two recurs ive calls right next to each other
; todo: currently, the rules allow to use the same name multiple times, but the prattparser can't deal with this

(defrecord PrattParser [name parsers]
  Parser
  (run [this input] (run-pratt this input 0))

  (map-success [this mapper]
    (map-success (->SimpleParser (fn [input] (run this input))) mapper)))



(defmacro def-op-parser
  {:clj-kondo/lint-as 'def} 
  [name] 
  `(def ~name (->PrattParser (quote ~name) (atom {}))))

(defn- op-inner-parser [first-name last-name middle result-expr options]
  (cond
    (and (not first-name) (not last-name)) ; leaf
    `(assoc (p-let ~middle ~result-expr) ::kind ::leaf)

    (and first-name (not last-name)) ; postfix
    (let [this (gensym "this")
          child (gensym "child")]
      (when-not (:precedence options) (throw (Exception. "postfix parser has to specify its precedence.")))
      `(assoc (parse-seq ~(make-seq-input middle)
                         :success-options {::result-fn (fn [~this ~child]
                                                         (let [~first-name ~child]
                                                           (let ~(make-let-bindings middle this)
                                                             ~result-expr)))
                                           ::left-bp ~(:precedence options)
                                           ::kind ::postfix})
              ::kind ::postfix))

    (and (not first-name) last-name) ; prefix
    (let [this (gensym "this")
          child (gensym "child")]
      (when-not (:precedence options) (throw (Exception. "prefix parser has to specify its precedence.")))
      `(assoc (parse-seq ~(make-seq-input middle)
                         :success-options {::result-fn (fn [~this ~child]
                                                         (let ~(make-let-bindings middle this)
                                                           (let [~last-name ~child]
                                                             ~result-expr)))
                                           ::right-bp ~(:precedence options)
                                           ::kind ::prefix})
              ::kind ::prefix))

    (and first-name last-name) ; infix
    (let [this (gensym "this")
          left-child (gensym "left-child")
          right-child (gensym "right-child")
          associativity (:associates options)
          right-bp #(case associativity
                      :left (+ % 0.1)
                      :right (- % 0.1))
          left-bp #(case associativity
                     :left (- % 0.1)
                     :right (+ % 0.1))]
      (when-not (:precedence options) (throw (Exception. "infix parser has to specify its precedence.")))
      (when-not (:associates options) (throw (Exception. "infix parser has to specify its associativity"))) 
      `(assoc (parse-seq ~(make-seq-input middle)
                         :success-options {::result-fn (fn [~this ~left-child ~right-child]
                                                         (let [~first-name ~left-child]
                                                           (let ~(make-let-bindings middle this)
                                                             (let [~last-name ~right-child]
                                                               ~result-expr))))
                                           ::right-bp ~(right-bp (:precedence options))
                                           ::left-bp ~(left-bp (:precedence options))
                                           ::kind ::infix})
              ::kind ::infix))))

(defmacro def-op [op-parser-name rule-name & stuff]
  (let [options (if (map? (first stuff)) (first stuff) {})
        stuff (if (map? (first stuff)) (rest stuff) stuff)
        defining-rules (first stuff)
        stuff (rest stuff)
        res-expr (first stuff)
        first-name (nth defining-rules 0)
        first-parser (nth defining-rules 1)
        last-name (nth defining-rules (- (count defining-rules) 2))
        last-parser (nth defining-rules (- (count defining-rules) 1))
        first-is-recur (= first-parser op-parser-name)
        last-is-recur (= last-parser op-parser-name)
        middle (drop (if first-is-recur 2 0) (drop-last (if last-is-recur 2 0) defining-rules))]
    (when-not (= 1 (count stuff)) (throw (Exception. "wrong number of arguments in def-op")))
    `(swap! (:parsers ~op-parser-name) #(assoc % (quote ~rule-name) ~(op-inner-parser (if first-is-recur first-name nil)
                                                                                      (if last-is-recur last-name nil)
                                                                                      middle
                                                                                      res-expr
                                                                                      options)))))