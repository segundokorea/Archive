(begin
  ; This comment here is okay
  (define n 12) ; So is this
  (define m n)

  ; This is also a comment
  ; But, it doesn't span multiple lines
  ; Oh, well. Let's define some stuff...
  (define test
    (lambda (n)
      (if (<= n 1) n
        (test (- n 1)))))

  (define fact
    (lambda (n)
      (if (<= n 1) 1
        (* n (fact (- n 1))))))

  (define succ
    (lambda (x)
      (+ x 1)))

  (define times
    (lambda (x y)
      (* x y)))

  ; Now, each line is evaluated, but only
  ; the toplevel expression will have its
  ; value returned, but we can use display
  ; to print things out
  (display (times 5 6))
  (display (cons 0 '(1 2 3)))
  (display (car '(1 2 3)))
  (display (cdr '(1 2 3)))
  (display (map succ '(1 2 3)))

  ; A list evaluates to itself if the head
  ; is not a function defined by the env:
  (true false)

  ; Let's just have some fun
  (set! n ; this is also okay
    20)
  (succ n)
  (test n)
  (display (fact n))
  (display (builtin.fact n))
  (display 'welcome-to-my-world)
  'nil nil heyo nil true false ; This is not a list...
  (display "About to do it again")
  (begin (+ 1 2) (/ y 2) (* 2 3) (display nil))
  (display '1234)
  (test (* -1 5))
  (display (let (z 1) (a 5) (+ z n)))
  (set! n 6)

  ; The toplevel begin-expression will
  ; return the value of its final form:
  (/ n m))
