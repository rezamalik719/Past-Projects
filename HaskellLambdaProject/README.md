## CSCI 4430 Programming Languages Assignment 1: Lambda Calculus Interpreter

### Contributors

Tyler Baughcome (baught) & Reza Malik (malikr3)

The contents of this zipfile comprise a set of Haskell scripts that fully reduce lambda calculus expressions. To reduce a file of lambda calculus expressions and output the reduced expressions to another file, use the following command in a prompt window where Haskell is installed.

```
runghc main.hs <input_file> <output_file>
```

Two sample input files are included, one provided by the instructors (sample.lambda) and another with a few more LC expressions (more_tests.lambda).

### Features

This project has the following features for manipulating and full reducing LC expressions.

#### Beta-Reduction

The _beta-reduce_ function in `main.hs` applies one expression to another and recursively beta-reduces the remaining expression until an irreducible expression remains. It relies on some of the following functions / features that help manipulate LC expressions.

#### Extract Free Variables

The _getFreeVariables_ function in `main.hs` acquires all free variables in the given LC expression and returns them in the format of a list.

#### Extract All Variables

The _getAllVariables_ function in `main.hs` acquires all variables in the given LC expression and returns them in the format of a list.

#### Variable Replacement / Substitution

The _replaceAll_ function takes three arguments: _arg_, _val_, and _exp_. All instances of _arg_ are substituted for _val_ in the LC expression _exp_.

#### Alpha-Renaming

The _alphaRename_ function in `main.hs` alpha-renames the given LC expression if necessary. Finding all free variables about to be applied, the _find_ function provides a new variable name if a free variable will be captured. This is achieved by adding a '0' character to the given parameter and all instances of that parameter in the LC expression.

#### Eta-Conversion

The _etaConvert_ function in `main.hs` recursively eta-converts the given LC expression until no more eta-conversion can be performed.

### Bugs

As of 09/25/2023, no bugs have been identified with the program.

### Resources

The attached program drew significant inspiration from the following github repository: https://github.com/jaeparkim/Programming-Languages/tree/master/pa1. The code here is by no means an exact copy, but a cursory work through their code will reveal resemblance to ours. If you have any questions or concerns about our use of this repo, please reach out to the both of us.
