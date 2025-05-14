# todos

## Possible errors:
- new line convention: CR LF vs Lf
- parser is very fragile
- parser does not handle errors.
- parser does not add position information to the nodes.
- variables are not marked as initialized when they are assigned by asnop.


## Better Building
- use maven/gradle/... to download the jar for clojure instead of having it in the repo.
- aot compile it.

## test case ideas
00 is not a valid token => should lead to parser error
--<expr> is valid and is -(-(<expr>)). does not mutate.