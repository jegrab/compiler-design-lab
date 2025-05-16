# Building and running the compiler

To build with make, run `make clean && make build`.
This aot-compiles the compiler and puts the generated class files into a directory named `classes`.
Alternatively, run the file `build.sh`. It also executes those commands.

Then, by running `run.sh <source-file> <out-file>`, you can run the compiler.

By running `jit.sh <source-file> <out-file>`, you can run the compiler without needing to build it before.

# Why it is build this way

With the current setup, the only tools needed to build and run the compiler are make and java.
The clojure compiler is contained in this repository as a jar in the `clojure` directory.
This makes it possible to run the compiler even on systems where clojure is not installed.
