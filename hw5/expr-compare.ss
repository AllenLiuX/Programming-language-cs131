#lang racket

(define LAMBDA (string->symbol "\u03BB"))


; helper, recursively build dict by pair of args and vals
(define (dict-generator keys vals)
  (cond
   [(null? keys) (hash)]
   [#t (hash-set (dict-generator (rest keys) (rest vals))
                 (first keys) (first vals))]
))


; translate vars after dict
(define (vars-after-dict vars dict)
  (cond
   [(null? vars) vars]
   [#t (cons (hash-ref dict (first vars) "NotFound")
             (vars-after-dict (rest vars) dict))]
))

(define (lambda? x)
  (or (equal? x 'lambda)(equal? x LAMBDA)))

(define (expr-trans expr cur-dict check-deeper)
  (cond
   [(null? expr) expr]
   [(not (list? expr))
    (if (equal?
        (hash-ref cur-dict expr "NotFound")
        "NotFound")
	expr
	(hash-ref cur-dict expr "NotFound"))]
   [(equal? (first expr) 'quote)
    (cons (first expr)
          (cons (cadr expr)
                (expr-trans (cddr expr) cur-dict check-deeper)))]
   [(and (list? (first expr)) (not check-deeper))    
    (cons (expr-trans (first expr) cur-dict check-deeper)
          (expr-trans (rest expr) cur-dict check-deeper))]
   [#t
    (let ([trans (hash-ref cur-dict (first expr) "NotFound")])
      (cond
       [(equal? trans "NotFound")
	(if (lambda? (first expr))
	    (cons (first expr)
	          (expr-trans (rest expr) cur-dict #t))
	    (cons (first expr)
	          (expr-trans (rest expr) cur-dict check-deeper)))]
       [(lambda? trans)
	(cons trans (expr-trans (rest expr) cur-dict #t))]
       [#t
	(cons trans (expr-trans (rest expr) cur-dict check-deeper))]))]
))

; Process lambda part
(define (lambda-recursor x y)
  (cond
   [(null? x) x]
   [(equal? (first x) (first y))
    (cons (first x) (lambda-recursor (rest x) (rest y)))]
   [#t (cons (string->symbol
             (string-append (symbol->string (first x))
                            "!"
                            (symbol->string (first y))))
	  (lambda-recursor (rest x) (rest y)))]
))

(define (replace-lambda-sym x y xargs yargs xexpr yexpr)
  (let ([lam
         (if (and (equal? (first x) 'lambda) (equal? (first y) 'lambda))
             'lambda 'λ)])
    (cons lam
          (cons (expr-compare xargs yargs)
                (cons (expr-compare xexpr yexpr) '() )))
))

(define (lambda-entry x y)
  (let ([xvars (cadr x)] [yvars (cadr y)])
    (cond
     [(not (equal? (length xvars) (length yvars)))
      (list 'if '% x y)]
     [#t (let ([varvals (lambda-recursor xvars yvars)])
	(let ([xdict (dict-generator xvars varvals)]
	      [ydict (dict-generator yvars varvals)])
	  (replace-lambda-sym x y
			  (vars-after-dict xvars xdict) (vars-after-dict yvars ydict)
			  (expr-trans (caddr x) xdict #f) (expr-trans (caddr y) ydict #f))))]
)))


;;;; main processes are below
(define (body-checker x y)
  (if (null? x) x
      (let ([x-head (first x)] [y-head (first y)])
        (cond
         [(equal? x-head y-head)
          (cons x-head (body-checker (rest x) (rest y)))]
         [(and (boolean? x-head) (boolean? y-head))
          (cons (if x-head '% '(not %)) (body-checker (rest x) (rest y)))]
         [(and (list? x-head) (list? y-head))
          (cons (expr-compare x-head y-head) (body-checker (rest x) (rest y)))]
         [#t (cons (list 'if '% x-head y-head)
                   (body-checker (rest x) (rest y)))]
))))


; cases that can be taken care by checking head
(define (head-checker x y)
  (let ([x-head (first x)] [y-head (first y)])
    (cond
     [(or (equal? x-head 'quote) (equal? y-head 'quote))
      (list 'if '% x y)]
     [(or (lambda? x-head) (lambda? y-head))
      (if (not (and (lambda? x-head) (lambda? y-head)))
	  (list 'if '% x y)
	  (lambda-entry x y))]
     [(equal? x-head y-head)
      (body-checker x y)]
     [(or (equal? x-head 'if) (equal? y-head 'if))
      (list 'if '% x y)]
     [(and (list? x-head) (list? y-head)) ; recursive step
      (cons (expr-compare x-head y-head) (expr-compare (rest x) (rest y)))]
     [#t (body-checker x y)]
)))

; base cases that can directly return
(define (expr-compare x y)
  (cond 
   [(equal? x y) x]
   [(and (boolean? x) (boolean? y))
    (if x '% '(not %))]
   [(or (not (list? x)) (not (list? y)))
    (list 'if '% x y)]
   [(not (equal? (length x) (length y)))
    (list 'if '% x y)]
   [#t (head-checker x y)]
))


(define (test-expr-compare x y) 
  (and
       (equal? (eval x)
               (eval (list 'let '((% #t)) (expr-compare x y))))
       (equal? (eval y)
               (eval (list 'let '((% #f)) (expr-compare x y))))
))

(define test-expr-x '(#t
                      (let b 3)
                      '(a b)
                      (lambda (a b c) (cons (cons(if a b c) 1) 2))
                      (lambda (i j k) (cons i j))
                      (quote 3 4)
                      (if 'a 'b #f)
                      (/ 1 0))) 
(define test-expr-y '(#f
                      (let a 3)
                      '(a c)
                      (lambda (a j k) (cons (cons(if a j k) 1) 3))
                      (λ (i m n) (list i m))
                      (quote 3 5)
                      (list 'a b #f)
                      (* 1 1)))

(expr-compare test-expr-x test-expr-y)

; test cases:
(expr-compare 12 12) ; ⇒  12
(expr-compare 12 20) ; ⇒  (if % 12 20)
(expr-compare #t #t) ; ⇒  #t
(expr-compare #f #f) ; ⇒  #f
(expr-compare #t #f) ; ⇒  %
(expr-compare #f #t) ; ⇒  (not %)
(expr-compare 'a '(cons a b)) ; ⇒  (if % a (cons a b))
(expr-compare '(cons a b) '(cons a b)) ; ⇒  (cons a b)
(expr-compare '(cons a lambda) '(cons a λ))  ;⇒  (cons a (if % lambda λ))
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c))) ; ⇒ (cons (cons a (if % b c)) (cons (if % b a) c))
(expr-compare '(cons a b) '(list a b));  ⇒  ((if % cons list) a b)
(expr-compare '(list) '(list a)) ; ⇒  (if % (list) (list a)
(expr-compare ''(a b) ''(a c)) ; ⇒  (if % '(a b) '(a c))
(expr-compare '(quote (a b)) '(quote (a c))) ; ⇒  (if % '(a b) '(a c))
(expr-compare '(quoth (a b)) '(quoth (a c))) ; ⇒  (quoth (a (if % b c)))
(expr-compare '(if x y z) '(if x z z)) ; ⇒  (if x (if % y z) z)
(expr-compare '(if x y z) '(g x y z)) ; ⇒ (if % (if x y z) (g x y z))

(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
  ;⇒ ((lambda (a) ((if % f g) a)) (if % 1 2))
(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
  ;⇒ ((λ (a) ((if % f g) a)) (if % 1 2))
(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
  ;⇒ ((lambda (a!b) a!b) (if % c d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
  ;⇒ (if % '((λ (a) a) c) '((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
  ;⇒ (+
  ;   (not %)
  ;   ((λ (a b!c) (f a b!c)) 1 2))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
  ;⇒ ((λ (a b) (f (if % a b) (if % b a))) 1 2)
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
  ;⇒ ((λ (a b!c) (f (if % a b!c) (if % b!c a)))
  ;   1 2)
(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
  ;⇒ ((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3)
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
  ;⇒ ((λ (a)
   ;   ((if % eq? eqv?)
    ;   a
     ;  ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
      ;  a (λ (a!b) (if % a!b a)))))
   ;  (lambda (b!a a!b) (b!a a!b)))

