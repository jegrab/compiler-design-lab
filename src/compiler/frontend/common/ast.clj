(ns compiler.frontend.common.ast
  (:require [clojure.spec.alpha :as s]
            [compiler.frontend.common.error :as err]))

(s/def ::kind keyword?)
(s/def ::children (s/coll-of keyword?))
(s/def ::node (s/and (s/keys :req [::kind ::children]
                             :opt [::err/errors])))


(defmulti pretty-print (fn [ast] (::kind ast)))

(defmulti check-after-parse 
  "takes an ast and does some check on the node"
  (fn [ast] (::kind ast)))

(defmethod check-after-parse :default [ast]
   (loop [ast ast
          children (::children ast)]
     (if (empty? children) ast
       (let [new-child (check-after-parse ((first children) ast))]
         (recur (assoc ast (first children) new-child)
                (rest children))))))

(defmulti execute "evaluates the node." (fn [ast state] (::kind ast)))

(defn collect-errors [ast] 
  (apply concat (map ::err/errors (tree-seq ::kind (fn [node] (map #(% node) (::children node))) ast))))
