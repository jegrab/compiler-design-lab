(ns compiler.core
  (:require [compiler.frontend.program :as p]
            [compiler.frontend.common.error :as err]
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
        {asts ::p/code 
         ers ::p/errors} (p/build-ast input-file)
        _ (when-not asts 
            (println "unknown fatal parser error")
            (exit-parsing))
        ers (clojure.set/union ers (apply concat (map ast/collect-errors asts)))
        parser-errors (filter #(= ::err/parser (::err/phase %)) ers)
        semantic-errors (filter #(= ::err/semantic-analysis (::err/phase %)) ers)
        pp (clojure.string/join "\n" (mapv ast/pretty-print asts))]
    (println "input: \n" (str pp) "\n")
    (println "parser errors: \n" (clojure.string/join "\n" (map ::err/message parser-errors)) "\n") 
    (println "semantic errors: \n" (clojure.string/join "\n" (map ::err/message semantic-errors)) "\n")
    (.flush *out*)
    (when-not (empty? parser-errors)
      (exit-parsing))
    (when-not (empty? semantic-errors)
      (exit-semantic-analysis)))
  (System/exit 0))

(apply main *command-line-args*)