#!/bin/bash

CLJ_JAR="clojure/clojure-1.12.0.jar"
SPEC_ALPHA_JAR="clojure/spec.alpha-0.5.238.jar"
CORE_SPECS_ALPHA_JAR="clojure/core.specs.alpha-0.4.74.jar"
CLASS_DIR="classes"

CLASSPATH="$CLJ_JAR:$SPEC_ALPHA_JAR:$CORE_SPECS_ALPHA_JAR:$CLASS_DIR"

inputFile="$1"
outputFile="${!#}"

java -cp "$CLASSPATH:$OUT_DIR" compiler.core $inputFile $outputFile.s \
&& \
gcc "$outputFile.s" -o "$outputFile"
