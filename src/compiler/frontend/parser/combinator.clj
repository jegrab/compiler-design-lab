(ns compiler.frontend.parser.combinator
  (:require [clojure.spec.alpha :as s]))

; a combinator is a function

(s/def ::input (s/and seq? (s/coll-of ::input-el)))
(s/def ::remaining ::input)
(s/def ::expected coll?)

(s/def ::success-result (s/keys :req [::result]))
(s/def ::failure-result (s/keys :req [::expected]))
(s/def ::comb-result (s/and
                      (s/keys :req [::remaining])
                      (s/or ::success-result ::failure-result)))

(s/def ::combinator (s/fspec
                     :args (s/cat :input ::input)
                     :ret ::comb-result))

(defn map [f comb]
  (fn [input]
    (let [res (comb input)]
      (if (::result res)
        (update res ::result f)
        res))))

(defn and-then
  "takes two parser combinators and executes them in one after the other.
   if one fails, returns the result of the first failing.
   if both succeed, returns the ::comb-result of the second 
   but with the ::result set to (combine-fn (::result res-first) (::result res-second)))"
  ([first second combine-fn]
   (fn [input]
     (let [res-first (first input)]
       (if (::result res-first)
         (let [res-second (second (::remaining res-first))]
           (if (::result res-second)
             (assoc second
                    ::result (combine-fn (::result res-first) (::result res-second)))
             res-second))
         res-first)))))

(defn either [a b]
  (fn [input]
    (let [a-res (a input)]
      (if (::result a-res)
        a-res
        (let [b-res (b input)]
          (if (::result b-res)
            b-res
            {::remaining input
             ::expected (concat (::expected a-res) (::expected b-res))}))))))

(defn accept
  ([accessor value expected] (accept #(= value (accessor %)) expected))
  ([pred expected]
   (fn [input]
     (if (pred (first input))
       {::remaining (rest input)
        ::result (first input)}
       {::remaining input
        ::expected [expected]}))))

(defn named
  "takes a name and a combinator and returns an equivalent combinator,
   but with ::expected set to [name]"
  [name combinator]
  (fn [input]
    (let [res (combinator input)]
      (if (::result res)
        res
        (assoc res
               ::expected [name])))))

(defn fail [expected]
  (fn [input] {::remaining input
               ::expected expected}))

(s/fdef fail
  :args (s/cat :expected ::expected)
  :ret ::combinator)


