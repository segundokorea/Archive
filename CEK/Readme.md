## CEK Interpreter

A simple CEK-style interpreter supporting a modest extension of the lambda calculus with *let*s, *if*s, and a few extra constant types, including numbers, booleans, and *nil*. Use `repl` to launch a line-by-line evaluator, like so (use Ctrl+C to exit):

    $ ./repl
    >> let c = false in let x = if c then c else nil in x
    => nil
    >> let fst = \x.\y.x in [[fst 1] 2]
    => 1
    >> if true then let x = 6 in \y.6 else nil
    => \y.6
    >> [\y.6 7]
    => 6
    >> [\y.6 nil]
    => 6

