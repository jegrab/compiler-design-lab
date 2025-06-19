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

(defn print-errors [parser-errors semantic-errors]
  (println "parser errors: \n" (clojure.string/join "\n" (map ::err/message parser-errors)) "\n")
  (println "semantic errors: \n" (clojure.string/join "\n" (map ::err/message semantic-errors)) "\n")
  (.flush *out*))

(defn- main [input-file-str output-file-str]
  (let [input-file (try (slurp input-file-str)
                        (catch java.io.FileNotFoundException e
                          (do
                            (println "input file '" input-file-str "' not found.")
                            (exit-illegal-arguments))))
        {decls ::p/decls
         errs ::p/errors
         main-id ::p/main-id} (p/frontend input-file)
        parser-errors (filter #(= ::err/parser (::err/phase %)) errs)
        semantic-errors (filter #(= ::err/semantic-analysis (::err/phase %)) errs)
        _ (when-not decls
            (print-errors parser-errors semantic-errors)
            (exit-parsing))
        _ (when-not (empty? parser-errors)
            (print-errors parser-errors semantic-errors)
            (exit-parsing))
        _ (when-not (empty? semantic-errors)
            (print-errors parser-errors semantic-errors)
            (throw-semantic-analysis))
        ;_ (println "input \n" (ast/pretty-print decls) "\n")
        ir (p/to-ir decls)
        asm (ir/make-code ir main-id)] 
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

