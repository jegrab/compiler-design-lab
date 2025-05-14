CLJ_JAR="clojure/clojure-1.12.0.jar"
SPEC_ALPHA_JAR="clojure/spec.alpha-0.5.238.jar"
CORE_SPECS_ALPHA_JAR="clojure/core.specs.alpha-0.4.74.jar"
SRC_DIR="src"
OUT_DIR="out"

CLASSPATH="$CLJ_JAR:$SPEC_ALPHA_JAR:$CORE_SPECS_ALPHA_JAR:$SRC_DIR"

inputFile="$1"
outputFile="${!#}"

java -cp "$CLASSPATH" clojure.main "$SRC_DIR/compiler/core.clj" "$inputFile" "$outputFile.s"
gcc "$outputFile.s" -o "$outputFile"


