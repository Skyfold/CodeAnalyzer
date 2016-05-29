# What is this tool?

This is a code analyzer that works on pseudocode. Its purpose is to give you an easy way to learn how to write first order logic constraints for code and how to write loop invariants. The solver understands code of the form:

```
// {-@ forall n, a (sum = n * a) @@ sum = (j * a) @-} 
sum := 0;
j := 0;
while (j /= n) {
    sum := sum + a;
    j := j + 1; 
}
```

Where the first order logic inside the `{-@ ... @@` is the postcondition the code must satisfy and the formulae inside the `@@ ... @-}` is the loop invariant for the first while loop in the code. To add another loop invariant, add another `@@` after the previous loop invariant like so:

```
// {-@ forall n, a (sum = n * a) @@ sum = (j * a) @@ sum = (n - a) @-} 
```

The analyzer is fully automated. It returns either an assignment of variables where the postcondition is violated or Q.E.D. The analyzer also returns the query it sent to the underlying SMT solver (mostly for my debugging purpose). For example, the code snippet above produces: 

```
forall n
       ,a (((0 = (0 * a)) && ((((0 /= n) && (0 = (0 * a))) -> ((0
a))) && ((~(0 /= n) && (0 = (0 * a))) -> (0 = (n * a))))))
Q.E.D.
```

I plan to create an online interface (when I find the time) so it is easy for others to play with it.

# Install

- Clone this repository
- Install [z3](https://github.com/Z3Prover/z3)
  - On mac you can `brew install z3`
- Install the [stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/) tool
  - On mac you can `brew install haskell-stack`
- change into the cloned directory
- `stack build`
  - Note, this will take awhile since it will download the Haskell compiler along with all the necessary libs.
- `stack exec codeAnalyzer <FilePath>`
  - where `<FilePath>` is replaced with the path to the file you wrote your pseudocode in.

# Examples

There are three example files I included in `testFiles`. To get the output I have above run this in the project directory:

```
stack exec codeAnalyzer testFiles/SimpleWhile
```
