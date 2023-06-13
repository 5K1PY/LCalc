# LCalc

Lambda calculus expression evaluator written in Haskell.
Final project for subject NPRG005 Nonprocedural programming. 

## Setup

Haskell is required for running this project.

## Running

Run the project  
```
$ runhaskell lcacl.hs
```
You will be asked to enter the expression. Expression can contain:
 - variables consisting of TODO
 - function in form: `(\x. expr)` where `x` is an argument and `expr` is an expression 
    - multiple arguments are not supported
 - sequence of functions and/or variables (separated by spaces and/or parenthesis) 

Then you will be repeatedly asked to choose a function to evaluate.
Enter the number of it and the program will do alpha-conversions (until necessary) and a beta-reduction.
