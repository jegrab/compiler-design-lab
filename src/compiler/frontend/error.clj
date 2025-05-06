(ns compiler.frontend.error
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defprotocol WithError
  (add-error [thing error] "annotate thing with the error.")
  (collect-errors [thing] "return a set of all errors that thing has."))


