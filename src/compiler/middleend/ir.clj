(ns compiler.middleend.ir
  (:require [clojure.spec.alpha :as s]))

(s/def ::instruction-name keyword?)
(s/def ::instruction (s/cat ::instruction-name))



; rval is constant or var
; [::assign var r-val]