(ns compiler.core
  (:require [compiler.frontend.program :as p]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.ast :as ast])
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
        asts (p/build-ast input-file)
        ers (apply concat (map ast/collect-errors asts))
        pp (clojure.string/join "\n" (mapv ast/pretty-print asts))]
    (println "ast: " asts)
    (println "errors: " ers)
    (println (str pp))) 
  (.flush *out*)
  (System/exit 0))

(apply main *command-line-args*)