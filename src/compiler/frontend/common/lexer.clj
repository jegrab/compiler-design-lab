(ns compiler.frontend.common.lexer
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.input :as in]
            [compiler.frontend.common.position :as pos]))

(s/def ::class keyword?)
(s/def ::kind keyword?)
(s/def ::source-string string?)
(s/def ::token (s/keys :req [::class ::kind ::source-string ::pos/span]
                       :opt [::err/errors]))

(def space-char #{\space \tab \newline \return})

(defn- skip-spaces [input]
  (if (space-char (in/current input))
    (skip-spaces (in/move input))
    input))

(defn- one-of-pred
  "takes a string of possible char values and returns a predicate.
   The predicate returns true if the given char is in the string and nil else"
  [string]
  (fn [char] (some #(= % char) string)))

(defn- make-one-char-token [input token-class]
  "creates a token that contains the current character of the input.
   the token gets the given class and an :undefined kind"
  [{::class token-class
    ::source-string (str (in/current input))
    ::pos/span (pos/span-from-to (in/current-position input)
                                 (in/current-position (in/move input)))}
   (in/move input)])

(defn- make-multi-char-token
  "creates a token that contains a prefix of the input (starting at the current position).
   the token gets the given class and an :undefined kind.
   for each character in the input, (state-fn character old-state) is called.
   if the returned state is a false value, the character is not included in the prefix.
   otherwise the character is included in the prefix.
   the initial state is nil.
   the prefix ends when the state-fn returns a false value."
  [input token-class state-fn]
  (loop [prefix []
         cur-in input
         state nil]
    (let [char (in/current cur-in)
          next-state (state-fn char state)]
      (if next-state
        (recur (conj prefix char)
               (in/move cur-in)
               next-state)
        [{::class token-class
          ::source-string (apply str prefix)
          ::pos/span (pos/span-from-to (in/current-position input)
                                   (in/current-position cur-in))}
         cur-in]))))

(defn- no-state-pred
  "creates a state function from the given predicate that has no state.
   Ignores the given state and just applies the predicate to the character."
  [predicate]
  (fn [char state] (predicate char)))

(defn- identifier-start? [char]
  (and
   (char? char)
   (or (Character/isLetter char) (= \_ char))))

(defn- identifier-char? [char]
  (and (char? char)
       (or (Character/isLetterOrDigit char) (= \_ char))))

(defn- optional-trailing-assignment []
  (fn [char state]
    (case state
      nil :first-char
      :first-char (if (= \= char)
                    :second-state
                    nil)
      :second-state nil)))

(defn- hex-char [char]
  ((one-of-pred "abcdefABCDEF0123456789") char))

(defn- starting-with-zero []
  (fn [char state]
    (case state
      nil :zero
      :zero (if (or (= \x char) (= \X char))
              :hex
              nil)
      :hex (if (hex-char char)
             :hex
             nil))))

(defn- lex-one-token [input]
  (condp #(%1 %2) (in/current input)
    nil? nil
    (one-of-pred "(){};") (make-one-char-token input ::separator)
    #(= % \=) (make-one-char-token input ::operator)
    (one-of-pred "+-*/%") (make-multi-char-token input ::operator (optional-trailing-assignment))
    #(= % \0) (assoc-in (make-multi-char-token input ::numerical-constant (starting-with-zero))
                        [0 ::num-kind] ::hex)
    (one-of-pred "123456789") (assoc-in
                               (make-multi-char-token input ::numerical-constant (no-state-pred (one-of-pred "0123456789")))
                               [0 ::num-kind] ::dec)
    ;operator-char? (make-multi-char-token input ::operator (no-state-pred operator-char?))
    identifier-start? (make-multi-char-token input ::identifier (no-state-pred identifier-char?))
    (make-one-char-token input ::error)))

(defn- lex-string [string]
  (loop [input (in/make-string-input string)
         tokens []]
    (let [[next rest] (lex-one-token (skip-spaces input))]
      (if next
        (recur rest (conj tokens next))
        tokens))))

(defmulti postprocess-token ::class)

(defn- add-kind [token kind]
  (assoc token ::kind kind))

(defmethod postprocess-token ::separator [token]
  (case (::source-string token)
    "(" (add-kind token ::left-parentheses)
    ")" (add-kind token ::right-parentheses)
    "{" (add-kind token ::left-brace)
    "}" (add-kind token ::right-brace)
    ";" (add-kind token ::semicolon)))

(defmethod postprocess-token ::operator [token]
  (case (::source-string token)
    "=" (add-kind token ::assign)
    "+" (add-kind token ::plus)
    "+=" (add-kind token ::plus-assign)
    "-" (add-kind token ::minus)
    "-=" (add-kind token ::minus-assign)
    "*" (add-kind token ::mul)
    "*=" (add-kind token ::mul-assign)
    "/" (add-kind token ::div)
    "/=" (add-kind token ::div-assign)
    "%" (add-kind token ::mod)
    "%=" (add-kind token ::mod-assign)))

(defmethod postprocess-token ::numerical-constant [token]
  (let [error {::err/phase ::lexer
               ::err/severity ::error
               :msg "illegal number format"}
        token (assoc token ::kind ::numerical-constant)]
    (try
      (let [parsed (edn/read-string (::source-string token))]
        (if (integer? parsed)
          (assoc token ::value parsed)
          (err/add-error token error)))
      (catch Exception e (err/add-error token error)))))


(def ^:private keyword-strings ["struct" "if" "else" "while" "for" "continue" "break" "return" "assert" "true" "false" "NULL" "print" "read" "alloc" "alloc_array" "int" "bool"
                                "void" "char" "string"])

(def ^:private keywords (loop [res {}
                               strs keyword-strings]
                          (if (empty? strs) res
                              (recur 
                               (assoc res (first strs) (keyword (str *ns*) (first strs)))
                               (rest strs)))))

(defmethod postprocess-token ::identifier [token]
  (cond
    (keywords (::source-string token))
    (assoc token
           ::class ::keyword
           ::kind (keywords (::source-string token)))

    (every? (fn [c] (or (<= (int \a) (int c) (int \z))
                        (<= (int \A) (int c) (int \Z))
                        (<= (int \0) (int c) (int \9))
                        (= c \_))) (::source-string token))
    (assoc token
           ::kind ::identifier)

    :else
    (assoc (err/add-error token {::err/phase ::lexer
                                 ::err/severity ::error
                                 :msg "identifiers can only contain the characters a-z, A-Z, 0-9 and _"})
           ::kind ::identifier)))

(defmethod postprocess-token ::error [token]
  (err/add-error
   (add-kind token ::error)
   {::err/phase ::lexer
    ::err/severity ::error
    :msg "illegal character"}))

(defn lex [string]
  (map postprocess-token (lex-string string)))

(s/fdef lex
  :args (s/cat :string ::source-string)
  :ret (s/coll-of ::token)
  :fn #(= (apply str (filter (fn [c ] (not (space-char c))) (-> % :args :string)))
          (apply str (map ::source-string (:ret %)))))