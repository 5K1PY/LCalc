# LCalc

Lambda calculus expression evaluator written in Haskell.
Final project for subject NPRG005 Nonprocedural programming. 

## Setup

Haskell is required for running this project. See more here: https://www.haskell.org/downloads/

## Running

Run the project  
```
$ runhaskell lcacl.hs
```
You will be asked to enter the expression. Expression can contain:
 - variables consisting of non-special characters (i.e. not parenthesis, backslash, dot and space)
 - function in form: `(\x. expr)` where `x` is a variable and `expr` is an expression 
    - multiple arguments are not supported
 - sequence of functions and/or variables (separated by spaces and/or parenthesis) 

Then you will be repeatedly asked to choose a function to evaluate.
Enter the number of it and the program will do alpha-conversions (until necessary) and a beta-reduction.

You can read more about lambda calculus here:
 - https://en.wikipedia.org/wiki/Lambda_calculus
 - https://brilliant.org/wiki/lambda-calculus/

## Architecture
Expression is first tokenized and then parsed into Absract syntax tree.

After user enters the number of callable for evaluation, it is first found
(by counting recursively how many callable functions are in each subtree).
Then alpha-conversion and beta-reduction is done by transforming one node above the callable.

Alpha-conversion renames colliding names of arguments
(colliding with free vars of argument) in the called function and all subojects.
Beta conversion likewise recursively replaces all occurences of
name of the argument with the argument.

Displaying is done recursively as combining your own representation with representation
of your subobjects.  
