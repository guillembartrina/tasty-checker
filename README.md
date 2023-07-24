# tasty-checker
 Reference checker for TASTy, the Scala 3 interchange format

## What it is
  __tasty-checker__ is a tool for checking that _tasty_ files adhere to the semantic specification of the TASTy format. Is is based on [__tasty-query__](https://github.com/scalacenter/tasty-query), which already checks the syntactic specification.

  The tool currently checks the following semantic invariants:
  * Whenever an expression of type `A` is used where a `B` is expected, check that `A` conforms to `B`
  * Whenever we have a type application `expr[...Ts]` or parametrized type `T[...Ts]`, check that the `...Ts` conform to the bounds of the type parameters of `expr` or `T`, respectively
  * When `A` extends `B` and they both have a type member `T`, check that `A.T` conforms to the bounds of `B.T`.
  * Whenever `A.m` overrides `B.m`, check that the result type of `A.m` conforms to that of `B.m`.
  * When `A` extends `B`, whenever `A.m` has the same erasure as `B.m`, check that `A.m` indeed overrides `B.m`.
  * Whenever a local reference to a definition `d` occurs, check that `d` is indeed in scope.
  * The type of bindings and wildcards within patterns is correct with regard to the context in which they occur.
  * The type of each expression is correctly calculated from the types of its subexpressions.


## Usage

### As an application

  Execute
  ```$ sbt run TARGET EXTRA```
  where
  * `TARGET` is a ';' separated list of paths to directories or JARs whose _tasty_ files are to be checked
  * `EXTRA` is a ';' separated list of paths to directories or JARs that complete the required context. One can use the special path '#' to indicate the default classpath used by sbt, which includes Scala and Java standard libs.

  Prints the list of semantic problems found.


### As a library

  Import the library.

  The entry point is `tastychecker.checkers.TASTyChecker`.

  It is defined by a list of `tastychecker.checks.TreeCheck`s and a `tastychecker.checks.TreeFilter`. Both can be constructed using the interface exposed by their respective companion object.

  `TASTyChecker` exposes the operation `check(targetPaths: List[Path], extraPaths: List[Path]): List[Result]`, where:
  * `targetPaths` is a list of paths to directories or JARs whose _tasty_ files are to be checked
  * `extraPaths` is a list of paths to directories or JARs that complete the required context

  And returns a list of `tastychecker.checkers.Result`s, representing the semantic problems found. This contains the top-level `Symbol` whose subtree contains the semantic problem, the `Entry` in which this symbol occurs and finally the `tastychecker.problems.Problem` itself.