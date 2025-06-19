(ns compiler.frontend.function
  (:require
   [clojure.string :as str]
   [compiler.frontend.common.error :as err]
   [compiler.frontend.common.id :as id]
   [compiler.frontend.common.type :as type]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.variable :as var]
   [compiler.frontend.common.namespace :as name]
   [compiler.frontend.expression :as expr]
   [compiler.middleend.ir :as ir]
   [compiler.frontend.integer :as int]))

; function type is map:
;{::args [list of arg types]
; ::return return-type}

(defn setup-env [env]
  (assoc env
         ::names {"print" :print
                  "read" :read
                  "flush" :flush}
         ::types {:print {::args [int/int-type]
                          ::return int/int-type}
                  :read {::args []
                         ::return int/int-type}
                  :flush {::args []
                          ::return int/int-type}}))

(defn- token [kind]
  (fn [tok]
    (= (::lex/kind tok) kind)))

(def parse-nonempty-args
  (p/p-let
   [first expr/parse-expr
    rest (p/many (p/p-let
                  [_ (token ::lex/comma)
                   arg expr/parse-expr]
                  arg))]
   (into [first] rest)))

(def parse-args
  (p/p-let
   [args (p/maybe parse-nonempty-args)]
   (if (nil? args)
     []
     args)))

(defn call-node [name-str args]
  {::ast/kind ::call
   ::ast/children [::args]
   ::function-name name-str
   ::args args})

(p/def-op expr/parse-expr call-normal
  {:precedence 100}
  [e expr/parse-expr
   _ (token ::lex/left-parentheses)
   args parse-args
   _ (token ::lex/right-parentheses)]
  (call-node (if (= ::var/identifier (::ast/kind e))
               (::var/name e)
               ::error-complex-expr)
             args))

(defmethod ast/check-after-parse ::call [c]
  (if-not (string? (::function-name c))
    (err/add-error c (err/make-parser-error "only identifiers allowed as functions."))
    c))

(p/def-op expr/parse-expr call-print
  [_ (token ::lex/print)
   _ (token ::lex/left-parentheses)
   args parse-args
   _ (token ::lex/right-parentheses)]
  (call-node "print" args))

(p/def-op expr/parse-expr call-read
  [_ (token ::lex/read)
   _ (token ::lex/left-parentheses)
   args parse-args
   _ (token ::lex/right-parentheses)]
  (call-node "read" args))

(p/def-op expr/parse-expr call-flush
  [_ (token ::lex/flush)
   _ (token ::lex/left-parentheses)
   args parse-args
   _ (token ::lex/right-parentheses)]
  (call-node "flush" args))

(defmethod ast/pretty-print ::call [call]
  (str (::function-name call) "("
       (str/join ", " (map ast/pretty-print (::args call))) ")"))

(defmethod name/resolve-names-expr ::call [call env]
  (let [call (assoc call ::args
                    (mapv #(name/resolve-names-expr % env) (::args call)))]
    (cond
      (not ((::names env) (::function-name call)))
      (err/add-error call (err/make-semantic-error (str "calling unknown function " (::function-name call))))

      :else
      (assoc call
             ::fun-id ((::names env) (::function-name call))))))

(defmethod expr/typecheck ::call [call env]
  (let [fun-type ((::types env) (::fun-id call))
        args-with-types (map #(expr/typecheck % env) (::args call))
        errs (if (not= (count args-with-types)
                       (count (::args fun-type)))
               #{(err/make-semantic-error (str "calling function " (::function-name call)
                                               " that expects " (count (::args fun-type))
                                               " arguments with " (count args-with-types)
                                               " arguments."))}
               #{})
        type-mismatches (filter identity
                                (map (fn [expected arg]
                                       (when-not (type/equals expected (::type/type arg))
                                         (err/make-semantic-error (str "type mismatch in function call. expected "
                                                                       expected
                                                                       " but got " (::type/type arg)))))
                                     (::args fun-type)
                                     args-with-types))
        errs (into errs type-mismatches)
        ret-type (or (::return fun-type) type/unknown)]
    (err/add-errors
     (assoc call
            ::args args-with-types
            ::type/type ret-type)
     errs)))

(defmethod name/check-initialization-expr ::call [call env]
  (assoc call
         ::args (map #(name/check-initialization-expr % env) (::args call))))

(defmethod expr/to-ir ::call [call res]
  (let [tmps-with-code (map (fn [arg] (let [tmp (id/make-tmp)]
                                        [(expr/to-ir arg tmp) tmp]))
                            (::args call))
        code (into [] (apply concat (map first tmps-with-code)))
        tmps (map second tmps-with-code)]
    (conj code
          [::ir/assign res (into [::ir/call (::fun-id call)]
                                 tmps)])))