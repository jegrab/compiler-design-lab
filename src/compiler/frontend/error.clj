(ns compiler.frontend.error
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol WithError
  (add-error [thing error] "annotate thing with the error.")
  (collect-errors [thing] "return a set of all errors that thing has."))

(defprotocol ErrorMessage
  (add-position [this span source-string])
  (add-phase [this phase]))

(defrecord StringMessageError [message]
  ErrorMessage
  (add-position [this span source-string]
    (assoc this ::span span)
    (assoc this ::source-string source-string))
  (add-phase [this phase]
    (assoc this ::phase phase)))

