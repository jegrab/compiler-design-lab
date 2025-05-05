(ns compiler.core
  (:gen-class))

(defn- main [& args]
  (println "hello world")
  (println args)
  (.flush *out*)
  (System/exit 0))

(apply main *command-line-args*)