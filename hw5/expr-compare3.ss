#lang racket

(define LAMBDA (string->symbol "\u03BB"))

(define (lambda? x)
  (member x '(lambda λ)))

; helper, recursively build dict by pair of args and vals
(define (build-dict keys vals)
  (cond
   [(null? keys) (hash)]
   [#t
    (hash-set (build-dict (cdr keys) (cdr vals)) (car keys) (car vals))]
))


(define (translate-vars vars dict)
  (cond
   [(null? vars) vars]
   [#t
    (cons (hash-ref dict (car vars) "NotFound")
          (translate-vars (cdr vars) dict))]
))


(define (translate-expr expr vardict nested)
  (cond
   [(null? expr) expr]
   [(not (list? expr))
    (if (equal? (hash-ref vardict expr "NotFound") "NotFound")
	expr
	(hash-ref vardict expr "NotFound"))]
   [(equal? (car expr) 'quote)
    (cons (car expr)
          (cons (cadr expr)
                (translate-expr (cddr expr) vardict nested)))]
   [(and (list? (car expr)) (not nested))    
    (cons (translate-expr (car expr) vardict nested)
          (translate-expr (cdr expr) vardict nested))]
   [#t
    (let ([trans (hash-ref vardict (car expr) "NotFound")])
      (cond
       [(equal? trans "NotFound")
	(if (lambda? (car expr))
	    (cons (car expr) (translate-expr (cdr expr) vardict #t))
	    (cons (car expr) (translate-expr (cdr expr) vardict nested)))]
       [(lambda? trans)
	(cons trans (translate-expr (cdr expr) vardict #t))]
       [#t
	(cons trans (translate-expr (cdr expr) vardict nested))]))]
))

; Process lambda part
(define (lambda-args x y)
  (cond
   [(null? x) x]
   [(equal? (car x) (car y))
    (cons (car x) (lambda-args (cdr x) (cdr y)))]
   [#t
    (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
	  (lambda-args (cdr x) (cdr y)))]
))

(define (process-lambda x y xargs yargs xexpr yexpr)
  (let ([lam-sym
         (if (and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
             'lambda 'λ)])
    (cons lam-sym
          (cons (expr-compare xargs yargs)
                (cons (expr-compare xexpr yexpr) '() )))
))

(define (lambda-heads x y)
  (let ([xvars (cadr x)] [yvars (cadr y)])
    (cond
     [(not (equal? (length xvars) (length yvars)))
      (list 'if '% x y)]
     [#t
      (let ([varvals (lambda-args xvars yvars)])
	(let ([xdict (build-dict xvars varvals)]
	      [ydict (build-dict yvars varvals)])
	  (process-lambda x
			  y
			  (translate-vars xvars xdict)
			  (translate-vars yvars ydict)
			  (translate-expr (caddr x) xdict #f)
			  (translate-expr (caddr y) ydict #f))))]
)))


(define (exam-body x y)
  (if (null? x)
      x
      (let ([xhead (car x)] [yhead (car y)])
	(cond
	 [(equal? xhead yhead)
	  (cons xhead (exam-body (cdr x) (cdr y)))]
	 [(and (boolean? xhead) (boolean? yhead)) 
	  (cons (if xhead '% '(not %)) (exam-body (cdr x) (cdr y)))]
	 [(and (list? xhead) (list? yhead))
	  (cons (expr-compare xhead yhead) (exam-body (cdr x) (cdr y)))]
	 [#t
	  (cons (list 'if '% xhead yhead) (exam-body (cdr x) (cdr y)))])
)))

(define (exam-heads x y)
  (let ([xhead (car x)] [yhead (car y)])
    (cond
     [(or (lambda? xhead) (lambda? yhead))
      (if (not (and (lambda? xhead) (lambda? yhead)))
	  (list 'if '% x y)
	  (lambda-heads x y))]
     [(or (equal? xhead 'quote) (equal? yhead 'quote))
      (list 'if '% x y)]
     [(equal? xhead yhead)
      (exam-body x y)]
     [(or (equal? xhead 'if) (equal? yhead 'if))
      (list 'if '% x y)]
     [(and (list? xhead) (list? yhead))
      (cons (expr-compare xhead yhead) (expr-compare (cdr x) (cdr y)))]
     [#t
      (exam-body x y)]
)))

(define (expr-compare x y)
  (cond 
   [(equal? x y) x]
   [(and (boolean? x) (boolean? y))
    (if x '% '(not %))]
   [(or (not (list? x)) (not (list? y)))
    (list 'if '% x y)]
   [(not (equal? (length x) (length y)))
    (list 'if '% x y)]
   [#t (exam-heads x y)]
))


(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval (list 'let '([% #t]) (expr-compare x y))))
       (equal? (eval y)
               (eval (list 'let '([% #f]) (expr-compare x y))))
))

(define test-expr-x '((lambda (a b c d) (cons (if a b c) d)) #t ((lambda (m n) (cons m n)) 8 24) (if #t 'w 'x) (+ 14 8))) 
(define test-expr-y '((lambda (a j k d) (cons (if a j k) d)) #f ((λ (v n) (list v n)) 8 23) (list #t 'w 'x) (- 14 8)))


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
