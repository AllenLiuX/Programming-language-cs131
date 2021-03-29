#lang racket
(define (c2 f) 
  (lambda (x) (
     lambda (y) (f x y)))
)

(((c2 cons) 'a) 'b)

(define (c3 f) 
  (lambda (x) (
      lambda (y) (f x y)))
)

(((c3 cons) 'a) 'b)

(define (g x)
  (let f ((y x))
     f))

(define (f (g 1)))