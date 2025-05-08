(ns compiler.core
  (:require [compiler.frontend.parser.lexer :as lex])
  (:gen-class))

(defn- exit-illegal-arguments []
  (.flush *out*)
  (System/exit 1))

(defn- exit-parsing []
  (.flush *out*)
  (System/exit 42))

(defn- exit-semantic-analysis [] (.flush *out*)(System/exit 7))


(defn- main [& args]
  (let [input-file-str (first args)
        output-file-str (last args)
        input-file (try (slurp input-file-str)
                        (catch java.io.FileNotFoundException e
                          (do
                            (println "input file '" input-file-str "' not found.")
                            (exit-illegal-arguments))))
        tokens (lex/lex input-file)]
    (println (str tokens))) 
  (.flush *out*)
  (System/exit 0))

(apply main *command-line-args*)