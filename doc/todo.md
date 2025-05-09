# todos

## (Possible) errors:
- new line convention: CR LF vs Lf
- in the lexer, 0123 is currently read as 0 123 and should be as 123.

## Better Building
- use maven/gradle/... to download the jar for clojure instead of having it in the repo.
- aot compile it.

## test case ideas
00 is not a valid token => should lead to parser error
--<expr> is valid and is -(-(<expr>)). does not mutate.