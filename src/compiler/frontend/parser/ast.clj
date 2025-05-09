(ns compiler.frontend.parser.ast
  (:require [clojure.spec.alpha :as s]
            [compiler.frontend.error :as err]
            [compiler.frontend.position :as pos]))

(s/def ::kind keyword?)
(s/def ::children (s/coll-of keyword?))
(s/def ::node (s/and (s/keys :req [::kind ::children ::pos/span]
                             :opt [::err/errors])))

