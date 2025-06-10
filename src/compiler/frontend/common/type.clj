(ns compiler.frontend.common.type
  (:require [compiler.frontend.common.parser :as p]))

(p/defmultiparser parse)

(defn simple-type [name-key]
  {::kind name-key})

(def error (simple-type ::error))

(def unknown (simple-type ::unknown))

(defn equals [a b]
  (let [ka (::kind a)
        kb (::kind b)]
    (cond (or (= ::error ka)
              (= ::error kb)
              (= ::unknown ka)
              (= ::unknown kb)) 
          true
          
          :else 
          (= a b))))