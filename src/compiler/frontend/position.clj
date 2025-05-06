(ns compiler.frontend.position)

(defprotocol Position
  "A position in the source code. Has line and column."
  (move [this] [this n] "increases the column of the position by n (default 1).")
  (nextline [this] "move to start of next line.")
  (span-to [this other] "creates a Span from this to other"))

(defprotocol Span)

(defrecord FileSpan [start end])

(defrecord FilePosition [^Integer line ^Integer column]
  Position
  (move [this] (FilePosition. line (inc column)))
  (move [this n] (FilePosition. line (+ column n)))
  (nextline [this] (FilePosition. (inc line) 1))
  (span-to [this other] (FileSpan. this other)))