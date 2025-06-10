(ns compiler.core
  (:require [compiler.frontend.program :as p]
            [compiler.frontend.common.error :as err]
            [compiler.frontend.common.lexer :as lex]
            [compiler.frontend.common.ast :as ast]
            [compiler.frontend.statement :as stmt]
            [compiler.middleend.ir :as ir])
  (:gen-class))

(defn- exit-illegal-arguments []
  (throw (ex-info "illegal arguments error" {:exitcode 1 :cause "illegal arguments"})))

(defn- exit-parsing []
  (throw (ex-info "parser error" {:exitcode 42 :cause "parser error"})))

(defn- throw-semantic-analysis []
  (throw (ex-info "semantic analysis error" {:exitcode 7 :cause "semantic error"})))

(defn- main [input-file-str output-file-str] 
  (let [input-file (try (slurp input-file-str)
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
        pp (clojure.string/join "\n" (mapv ast/pretty-print asts))
        ir (when asts (p/to-ir asts))
        asm (ir/make-code ir)]
    (println "ast:" asts)
    (println "input: \n" (str pp) "\n")
    (println "parser errors: \n" (clojure.string/join "\n" (map ::err/message parser-errors)) "\n")
    (println "semantic errors: \n" (clojure.string/join "\n" (map ::err/message semantic-errors)) "\n")
    (.flush *out*)
    (when-not (empty? parser-errors)
      (exit-parsing))
    (when-not (empty? semantic-errors)
      (throw-semantic-analysis))
    (spit output-file-str asm)))


(defn -main [& args]
  (when (not= 2 (count args))
    (println "needs exactly two command line arguments. got " args)
    (exit-illegal-arguments))
  (try
    (println "input: " (first args) " output: " (second args))
    (main (first args) (second args))
    (System/exit 0)
    (catch clojure.lang.ExceptionInfo e
      (println "error: " (-> e ex-data :cause))
      (println "exiting with " (-> e ex-data :exitcode))
      (System/exit (-> e ex-data :exitcode)))))

