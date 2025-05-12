(ns compiler.frontend.common.input
  (:require [compiler.frontend.common.position :as pos]))

(defprotocol Input
  "An input to a program that deals with positions in source code"
  (current-position [input])
  (current [input] "returns the element at the current position")
  (following [input] "returns the element at the next position")
  (move [input] [input n] "moves the current position by n forward (default is 1)"))


(defn- newline? [char] (#{\newline \return} char))

(deftype StringInput [string position ^int index]
  Input
  (current-position [this] position)
  (current [this] (get string index))
  (following [this] (get string (inc index)))
  (move [this]
    (if (newline? (current this))
      (StringInput. string (pos/next-line position) (inc index))
      (StringInput. string (pos/move position) (inc index))))
  (move [this n]
    (cond
      (< n 0) (throw (IllegalArgumentException. (str "Input can only move forward. " n " is not a valid step count.")))
      (= n 0 this)
      (move (move this) (dec n)))))

(defn make-string-input [string]
  (->StringInput string pos/initial-position 0))