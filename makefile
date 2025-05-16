CLJ_JAR=clojure/clojure-1.12.0.jar
SPEC_ALPHA_JAR=clojure/spec.alpha-0.5.238.jar
CORE_SPECS_ALPHA_JAR=clojure/core.specs.alpha-0.4.74.jar
SRC_DIR=src
OUT_DIR=classes
 
CLASSPATH=$(CLJ_JAR):$(SPEC_ALPHA_JAR):$(CORE_SPECS_ALPHA_JAR):$(SRC_DIR)

build:
	mkdir -p $(OUT_DIR)
	java -cp "$(CLASSPATH)" clojure.main -e "(compile 'compiler.core)"

interpret:
	java -cp "$(CLASSPATH)" clojure.main $(SRC_DIR)/compiler/core.clj justSomeArg

run:
	java -cp "$(CLASSPATH):$(OUT_DIR)" compiler.core $(ARGS)

clean:
	rm -rf $(OUT_DIR)