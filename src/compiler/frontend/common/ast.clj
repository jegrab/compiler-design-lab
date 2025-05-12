(ns compiler.frontend.common.ast
  (:require [clojure.spec.alpha :as s]
            [compiler.frontend.common.error :as err]))

(s/def ::kind keyword?)
(s/def ::children (s/coll-of keyword?))
(s/def ::node (s/and (s/keys :req [::kind ::children]
                             :opt [::err/errors])))


(defmulti pretty-print (fn [ast] (::kind ast)))
(defmulti semantic-analysis 
  "takes an ast and a state and returns [new-ast new-state]"
  (fn [ast state] (::kind ast)))

(defmulti execute "evaluates the node." (fn [ast state] (::kind ast)))

(defn collect-errors [ast] 
  (concat (apply concat (map collect-errors (map #(% ast) (::children ast))))
          (::err/errors ast)))
