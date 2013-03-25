## TScheem

Extension of **TScheem**, a toy language introduced by [Nathan&#8217;s University PL101: Create Your Own Programming Language](http://nathansuniversity.com/). This implementation features a richer syntax supporting comments and several supplementary literals, including strings, booleans, and `nil` (corresponding to JavaScript&#8217;s `null`). Type inference (not yet implemented) will be based on a simplified Hindley-Milner method.

To start an interactive REPL, just execute `scheem` (type "quit" to exit), or pass in the name of a program to have its contents evaluated:

    $ ./scheem
    %> ; This is still a comment
    %> (display "Hello, world!")
    Hello, world!
    %> (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
    %> (fact 5)
    => 120
    %> (+ (fact 5) (* 2 3))
    => 126
    %> (+ (fact 5) (cons 0 '(1 2 3)))
    => 1200,1,2,3
    %> quit
    Bye!

    $ ./scheem program.scm
    30
    [ 0, 1, 2, 3 ]
    1
    [ 2, 3 ]
    [ 2, 3, 4 ]
    2432902008176640000
    2432902008176640000
    welcome-to-my-world
    About to do it again
    nil
    1234
    21
    ===> 0.5
