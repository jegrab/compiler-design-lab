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