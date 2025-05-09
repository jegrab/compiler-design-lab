(require '[clojure.spec.alpha :as s]
         '[clojure.spec.gen.alpha :as gen]
         '[clojure.spec.test.alpha :as stest]
         '[compiler.frontend.parser.lexer :as lex]
         '[compiler.frontend.parser.combinator :as comb])



(def source-string-gen
  (gen/fmap (partial apply str)
            (gen/vector (gen/one-of [(gen/char-alpha)
                                     (gen/char-ascii)
                                     (gen/char-alphanumeric)
                                     (gen/elements [\space \tab \space \newline \space])
                                     (gen/char)
                                     (gen/elements [\! \@ \# \$ \% \^ \& \* \- \_ \= \+])])
                        0 1000)))

(s/def ::lex/source-string
  (s/with-gen string?
    (fn [] source-string-gen)))


(gen/sample (s/gen ::lex/source-string))

(stest/check `lex/lex)

(stest/check `comb/fail)