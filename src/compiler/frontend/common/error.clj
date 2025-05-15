(ns compiler.frontend.common.error
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(s/def ::phase keyword?)
(s/def ::severity keyword?)
(s/def ::message (s/keys :req [::phase ::severity]))
(s/def ::errors (s/coll-of ::message))

(defn add-error [this error]
  (assoc this ::errors (set/union #{error}
                                  (::errors this))))

(defn add-errors [this errors]
  (if (empty? errors)
    this
    (assoc this ::errors (set/union errors
                                    (::errors this)))))

(defn has-error? [thing] (not (empty? (::errors thing))))

(defn make-parser-error [message] 
  {::phase ::parser
   ::severity ::error
   ::message message})

(defn make-semantic-error [message]
  {::phase ::semantic-analysis
   ::severity ::error
   ::message message})