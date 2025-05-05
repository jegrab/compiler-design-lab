(ns compiler.core
  (:gen-class))

(defn- main [& args]
  (print "hello world")
  (print args)
  (System/exit 0))