(ns compiler.frontend.function
  (:require
   [clojure.string :as str]
   [compiler.frontend.common.error :as err]
   [compiler.frontend.common.id :as id]
   [compiler.frontend.common.type :as type]
   [compiler.frontend.common.ast :as ast]
   [compiler.frontend.common.lexer :as lex]
   [compiler.frontend.common.parser :as p]
   [compiler.frontend.toplevel :as top]
   [compiler.frontend.variable :as var]
   [compiler.frontend.block :as block]
   [compiler.frontend.common.namespace :as name]
   [compiler.frontend.expression :as expr]
   [compiler.middleend.oldir :as oldir]
   [compiler.middleend.ir :as ir] 
   [compiler.middleend.jump :as jump-ir]
   [compiler.frontend.integer :as int]
   [compiler.frontend.statement :as stmt]))

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
          [::oldir/assign res (into [::oldir/call (::fun-id call)]
                                 tmps)])))


(p/defrule stmt/parse-statement ::return
  [_ (token ::lex/return)
   ret-expr expr/parse-expr
   _ (token ::lex/semicolon)]
  {::ast/kind ::return
   ::ast/children [::ret-expr]
   ::ret-expr ret-expr})

(defmethod ast/pretty-print ::return [ret]
  (str "return " (ast/pretty-print (::ret-expr ret))))

(defmethod name/resolve-names-stmt ::return [ret env]
  [(assoc ret
          ::ret-expr (name/resolve-names-expr (::ret-expr ret) env))
   env])

(defmethod stmt/to-ir ::return [ret]
  (let [tmp (id/make-tmp)]
    (into
     (expr/to-ir (::ret-expr ret) tmp)
     (jump-ir/return tmp))))

(defmethod ast/gen-ir ::return [state ret]
  (ir/set-cont
   (ast/gen-ir (::ret-expr ret) (assoc state ::ir/target ::ir/ret))
   (ir/return)))

(defmethod stmt/typecheck ::return [ret env]
  (let [decl-type (env ::ret-type)
        new-expr (expr/typecheck (::ret-expr ret) env)
        actual-type (::type/type new-expr)
        new-ret (assoc ret ::ret-expr new-expr)]
    [(if (type/equals decl-type actual-type)
       new-ret
       (err/add-error new-ret (err/make-semantic-error (str "type mismatch. should return " decl-type " but returns " actual-type))))
     env]))

(defmethod stmt/returns ::return [_] true)

(defmethod name/check-initialization-stmt ::return [ret env]
  (let [expr (name/check-initialization-expr (::ret-expr ret) env)
        env (assoc env
                   ::name/initialized (clojure.set/union  (::name/initialized env)
                                                          (::name/defined env)))]
    [(assoc ret ::ret-expr expr)
     env]))

(defmethod stmt/ends-flow ::return [_] true)

(defn param-node [type name]
  {::ast/kind ::param
   ::ast/children []
   ::type type
   ::name name})

(def parse-nonempty-params
  (p/p-let
   [first (p/p-let [t type/parse
                    i (token ::lex/identifier)]
                   (param-node t (::lex/source-string i)))
    rest (p/many (p/p-let
                  [_ (token ::lex/comma)
                   t type/parse
                   i (token ::lex/identifier)]
                  (param-node t (::lex/source-string i))))]
   (into [first] rest)))

(def parse-params
  (p/p-let
   [args (p/maybe parse-nonempty-params)]
   (if (nil? args)
     []
     args)))

(p/defrule top/toplevel ::function-def
  [t type/parse
   name (token ::lex/identifier)
   _ (token ::lex/left-parentheses)
   params parse-params
   _ (token ::lex/right-parentheses)
   body block/parse-block]
  {::ast/kind ::function-def
   ::ast/children [::body ::params]
   ::name (::lex/source-string name)
   ::body body
   ::params params
   ::ret-type t})

(defmethod ast/pretty-print ::function-def [def]
  (str (::ret-type def)
       " "
       (::name def)
       "("
       (str/join ", " (map #(str (::type %) " " (::name %))
                           (::params def)))
       ")\n"
       (ast/pretty-print (::body def))
       "\n\n"))

(defmethod top/collect-names ::function-def [def env]
  (let [name (::name def)
        id (id/make-var)
        type {::args (mapv ::type (::params def))
              ::return (::ret-type def)}
        redef? ((::names env) (::name def))
        def (if redef? (err/add-error def (err/make-semantic-error (str "function " (::name def) " is allready defined")))
                def)] 
    [(assoc def ::id id)
     (assoc env
            ::names (assoc (::names env) name id)
            ::types (assoc (::types env) id type))]))

(defmethod name/resolve-names-stmt ::function-def [def env]
  (let [[new-params body-env] (loop [env env
                                new-params []
                                params (::params def)]
                           (if (empty? params) [new-params env]
                               (let [p (first params)
                                     [p e] (var/declare-var (::name p) p env)]
                                 (recur e
                                        (conj new-params p)
                                        (rest params)))))
        [new-body _] (name/resolve-names-stmt (::body def) body-env)]
    [(assoc def
            ::body new-body
            ::params new-params)
     env]))

(defmethod stmt/typecheck ::function-def [def env]
  (let [env (assoc env ::ret-type (::ret-type def))
        env (reduce (fn [env param]
                      (var/declare-var-type param (::type param) env))
                    env (::params def))
        [new-body _] (stmt/typecheck (::body def) env)]
    [(assoc def
            ::body new-body)
     env]))

(defmethod name/check-initialization-stmt ::function-def [def env]
  (let [env (reduce (fn [env param]
                      (name/initialize (::var/id param) env))
                    env (::params def))
        [body env] (name/check-initialization-stmt (::body def) env)
        has-return (stmt/returns (::body def))
        def (if-not has-return
              (err/add-error def (err/make-semantic-error (str "missing return statement in function " (::name def))))
              def)]
    [(assoc def
            ::body body)
     env]))

(defmethod top/to-ir ::function-def [def]
  (let [block (ir/fun-block (::id def) (map ::var/id (::params def)))]
    (ast/gen-ir (::body def) block)))

(defn is-main [def]
  (= "main" (::name def)))

(defn check-main [def env]
  (if-not (is-main def)
    [def env]
    (let [ret-type (::ret-type def)
          params (::params def)
          errs #{}
          errs (if-not (empty? params)
                 (conj errs (err/make-semantic-error (str "main must not have any paramters, but is declared with " (count params) " parameters."))
                       errs)
                 errs)
          errs (if-not (type/equals ret-type int/int-type)
                 (conj errs (err/make-semantic-error (str "main must return " int/int-type ", but returns " ret-type "."))
                       errs)
                 errs)]
      [(err/add-errors def errs) env])))