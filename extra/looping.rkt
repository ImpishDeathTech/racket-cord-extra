#lang racket

; An iterative loop
(define-syntax-rule (sigma [[name-clause start-expr] limit-expr iter-expr]
                           stx-expr ...)
  (let ([name-clause start-expr])
    (let loop ()
      (unless limit-expr
        stx-expr ...
        (set! name-clause iter-expr)
        (loop)))))

; A loop that runs while a condition expression is true
(define-syntax-rule (while cond-expr stx-expr ...)
  (let loop ()
    (when cond-expr
      stx-expr ...
      (loop))))

; A loop that runs until a limit expression is true
(define-syntax-rule (until limit-expr stx-expr ...)
  (let loop ()
    (unless limit-expr
      stx-expr ...
      (loop))))

; An infinate loop
(define-syntax-rule (repeat stx-expr ...)
  (let loop ()
    stx-expr ...
    (loop)))

(provide (all-defined-out))