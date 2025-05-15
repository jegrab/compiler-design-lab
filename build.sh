#!/bin/bash

CLJ_JAR=clojure/clojure-1.12.0.jar
SPEC_ALPHA_JAR=clojure/spec.alpha-0.5.238.jar
CORE_SPECS_ALPHA_JAR=clojure/core.specs.alpha-0.4.74.jar
SRC_DIR=src

CLASSPATH=$CLJ_JAR:$SPEC_ALPHA_JAR:$CORE_SPECS_ALPHA_JAR:$SRC_DIR

java -cp $CLASSPATH clojure.main -e "(compile 'testname.core)"
