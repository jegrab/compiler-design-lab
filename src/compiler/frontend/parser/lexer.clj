(ns compiler.frontend.parser.lexer
  (:require [compiler.frontend.error :as err]
            [compiler.frontend.parser.input :as in]
            [clojure.set :as set]
            [compiler.frontend.position :as pos]))

(defrecord Token [class kind source-string span]
  err/WithError
  (add-error [this error]
    (assoc this ::err/errors (set/union #{error}
                                        (::err/errors this))))
  (collect-errors [this] (::err/errors this)))

(defn- skip-spaces [input]
  (if (#{\space \tab \newline \return} (in/current input))
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
  [(Token. token-class
           :unknown
           (str (in/current input))
           (pos/span-to (in/current-position input)
                        (in/current-position (in/move input))))
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
        [(Token. token-class 
                 :unknown
                 (apply str prefix)
                 (pos/span-to (in/current-position input)
                              (in/current-position cur-in)))
         cur-in]))))

(defn- no-state-pred 
  "creates a state function from the given predicate that has no state.
   Ignores the given state and just applies the predicate to the character."
  [predicate]
  (fn [char state] (predicate char)))

(def operator-char? (one-of-pred "+-*/!?~%.<>=&^|:"))

(defn- identifier-start? [char]
  (and 
   (char? char)
   (or (Character/isLetter char) (= \_ char))))

(defn- identifier-char? [char]
  (and (char? char) 
       (or (Character/isLetterOrDigit char) (= \_ char))))

(defn- lex-one-token [input]
  (condp #(%1 %2) (in/current input)
    nil? nil
    (one-of-pred "(){};") (make-one-char-token input ::separator)
    operator-char? (make-multi-char-token input ::operator (no-state-pred operator-char?))
    identifier-start? (make-multi-char-token input ::operator (no-state-pred identifier-char?))))

(defn- lex-string [string]
  (loop [input (in/make-string-input string)
         tokens []]
    (let [[next rest] (lex-one-token (skip-spaces input))]
      (if next
        (recur rest (conj tokens next))
        tokens))))

(defn lex [string]
  (lex-string string))