(ns compiler.frontend.common.position
  (:require [clojure.spec.alpha :as s]))

(s/def ::line integer?)
(s/def ::column integer?)
(s/def ::position (s/keys :req [::line ::column]))
(s/def ::start ::position)
(s/def ::end ::position)
(s/def ::span (s/keys :req [::start ::end]))

(defn move
  "increases the column of the position by n (default 1)."
  ([pos] (update pos ::column inc))
  ([pos n] (update pos ::column #(+ n %))))

(defn next-line
  "move the position to start of next line. (increase the line and set the column to 1)"
  [pos] (assoc pos
               ::column 1
               ::line (inc (::line pos))))

(defn span-from-to [start end] {::start start ::end end})

(defn merge-spans [start-span end-span] {::start (::start start-span) ::end (::end end-span)})

(def initial-position {::line 1 ::column 1})