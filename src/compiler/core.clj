(ns compiler.core
  (:require [compiler.frontend.parser.lexer :as lex])
  (:gen-class))

(defn- main [& args]
  (println "args: " args)
  (println "lexed: \n"
           (lex/lex (str args)))
  (.flush *out*)
  (System/exit 0))

(apply main *command-line-args*)