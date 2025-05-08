(ns compiler.frontend.parser.ast
  (:require [clojure.spec.alpha :as s]
            [compiler.frontend.error :as err]))

(s/def ::kind keyword?)
(s/def ::children (s/coll-of keyword?))
(s/def ::node (s/and (s/keys :req [::kind ::children]
                             :opt [::err/errors])))

(defrecord node [kind children span]
  err/WithError
  (add-error [this error]
    (assoc this ::err/errors (set/union #{(err/add-position error span source-string)}
                                        (::err/errors this)))))



