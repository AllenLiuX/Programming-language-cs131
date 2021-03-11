#lang racket

(define LAMBDA (string->symbol "\u03BB"))

(define (lambda? x)
  (member x '(lambda λ)))

;Once we have translated the lambda arguments and expressions, we now run them through
;expr-compare as if they were not lambdas. Here, we also distinguish which of 'lambda'
;or 'λ' we should be using.
(define (process-lambda x y xargs yargs xexpr yexpr)
  (let ([lam-sym (if (and (equal? (car x) 'lambda) (equal? (car y) 'lambda)) 'lambda 'λ)])
    (cons lam-sym (cons (expr-compare xargs yargs) (cons (expr-compare xexpr yexpr) '() )))
))

;Helper function that takes the arguments of a lambda and its specific dictionary and
;translates them all recursively.
(define (translate-vars var vardict)
  (cond
   [(null? var) var]
   [#t
    ;"NOPE" will never match here because we mapped all variables to at least itself
    (cons (hash-ref vardict (car var) "NOPE") (translate-vars (cdr var) vardict))]
))

;Helper function that takes the expression of a lambda, its specific dictionary, and a
;boolean value and translates the values recursively.
(define (translate-expr expr vardict nested)
  (cond
   ;If expr is empty, return the empty list.
   [(null? expr) expr]
   ;If expr is not a list, then the expr is a single value, so we need only translate that one value.
   [(not (list? expr))
    (if (equal? (hash-ref vardict expr "NOPE") "NOPE")
	expr
	(hash-ref vardict expr "NOPE"))]
   ;If we run into a quote, then we skip checking it for translation and go on.
   [(equal? (car expr) 'quote)
    (cons (car expr) (cons (cadr expr) (translate-expr (cddr expr) vardict nested)))]
   ;If we run into a list, we first check the boolean value 'nested'.  If nested is true, we are
   ;within a nested lambda, and we do not want to change those values.
   [(and (list? (car expr)) (not nested))    
    (cons (translate-expr (car expr) vardict nested) (translate-expr (cdr expr) vardict nested))]
   [#t
    (let ([trans (hash-ref vardict (car expr) "NOPE")])
      (cond
       ;"NOPE" can happen here because it is acceptable for the expression to introduce things
       ;not seen in the arguments.
       [(equal? trans "NOPE")
	(if (lambda? (car expr))
	    ;If true, we have found a nested lambda and we change nested to true.
	    (cons (car expr) (translate-expr (cdr expr) vardict #t))
	    (cons (car expr) (translate-expr (cdr expr) vardict nested)))]
       [(lambda? trans)
	(cons trans (translate-expr (cdr expr) vardict #t))]
       [#t
	;There is a translated value that is not lambda.
	(cons trans (translate-expr (cdr expr) vardict nested))]))]
))

;Helper function that filters bad lambdas early and creates the dicionaries.
(define (lambda-heads x y)
  ;Set xvars and yvars equal to their respective arguments
  (let ([xvars (cadr x)] [yvars (cadr y)])
    (cond
     ;If there are a different amount of arguments, they are clearly different.
     [(not (equal? (length xvars) (length yvars)))
      (list 'if '% x y)]
     [#t
      ;varvals contains the arguments in order and indicates if there are different
      ;names for the same bound variable (such as b!c)
      (let ([varvals (lambda-args xvars yvars)])
	;Build the two dictionaries based on varvals
	(let ([xdict (build-dict xvars varvals)]
	      [ydict (build-dict yvars varvals)])
	  (process-lambda x
			  y
			  (translate-vars xvars xdict) ;send translated values of arguments/
			  (translate-vars yvars ydict) ;expressions to process-lambda
			  (translate-expr (caddr x) xdict #f)  ;Initialize nested to false
			  (translate-expr (caddr y) ydict #f))))]
)))

;Helper function that takes the arguments and the list varvals and creates a dictionary
;using a hashmap recursively.
(define (build-dict args varvals)
  (cond
   ;If args is empty, we have iterated through all of them already, so we create the hashmap.
   [(null? args) (hash)]
   ;Else, we add another entry to the dictionary using (argument value)->(varval value).
   [#t
    (hash-set (build-dict (cdr args) (cdr varvals)) (car args) (car varvals))]
))

;Helper function that takes in the arguments of the differing lambda functions and creates
;a list detailing where they are the same and where they differ recursively.
(define (lambda-args x y)
  (cond
   ;If x is empty, we have iterated through all the arguments and return the empty list.
   [(null? x) x]
   ;If the heads are equal, we add the value to the list by itself, which indicates that they
   ;are the same.
   [(equal? (car x) (car y))
    (cons (car x) (lambda-args (cdr x) (cdr y)))]
   ;If they are not equal, we convert both of them to strings, then append them together with
   ;an ! between them, then convert them back to symbols.  We then add this value to the list.
   [#t
    (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
	  (lambda-args (cdr x) (cdr y)))]
))

;Helper function that recursively deals with non-lambdas.
(define (exam-body x y)
  ;If one is empty, we return it.
  (if (null? x)
      x
      (let ([xhead (car x)] [yhead (car y)])
	(cond
	 ;If the first elements are equal, we recursively call exam-body with the remaining list.
	 [(equal? xhead yhead)
	  (cons xhead (exam-body (cdr x) (cdr y)))]
	 ;If they're both booleans, we return differently.
	 [(and (boolean? xhead) (boolean? yhead)) 
	  (cons (if xhead '% '(not %)) (exam-body (cdr x) (cdr y)))]
	 ;If they're both lists, we call expr-compare on them because exam-body deals mainly with
	 ;singleton values.  We recursively call exam-body on the rest of the lists.
	 [(and (list? xhead) (list? yhead))
	  (cons (expr-compare xhead yhead) (exam-body (cdr x) (cdr y)))]
	 ;Else, it means the two heads are different singleton values, so we indicate such and
	 ;recursively call exam-body on the rest.
	 [#t
	  (cons (list 'if '% xhead yhead) (exam-body (cdr x) (cdr y)))])
)))

;Helper function that deals with the more complicated cases neglected in expr-compare by examining
;the heads.
(define (exam-heads x y)
  ;Set the values of xhead and yhead accordingly.
  (let ([xhead (car x)] [yhead (car y)])
    (cond
     ;If either head is a lambda.
     [(or (lambda? xhead) (lambda? yhead))
      (if (not (and (lambda? xhead) (lambda? yhead)))
	  ;If only one is a lambda, then they are different so we return accordingly.
	  (list 'if '% x y)
	  ;If they are both lambdas, we call lambda-heads instead of exam-body because lambdas are
	  ;much more complicated to deal with.
	  (lambda-heads x y))]
     ;If they're both quotes, we don't touch them and just return accordingly.
     [(or (equal? xhead 'quote) (equal? yhead 'quote))
      (list 'if '% x y)]
     ;If the heads are equal, we send them to exam-body.
     [(equal? xhead yhead)
      (exam-body x y)]
     ;If one of them is an if, then we know they are different because of the above case.  We deal
     ;with this difference accordingly.
     [(or (equal? xhead 'if) (equal? yhead 'if))
      (list 'if '% x y)]
     ;If both are lists, then we call expr-compare on both and return the combination.
     [(and (list? xhead) (list? yhead))
      (cons (expr-compare xhead yhead) (expr-compare (cdr x) (cdr y)))]
     ;Else, we have two heads that are different but are not 'if', so we call exam-body on them.
     [#t
      (exam-body x y)]
)))

(define (expr-compare x y)
  (cond 
   ;If they're equal, we simply return one of them (taken from TA hint code).
   [(equal? x y) x]
   ;Deals with the special case of booleans (taken from TA hint code).
   [(and (boolean? x) (boolean? y))
    (if x '% '(not %))]
   ;If one is not a list (taken from TA hint code).
   [(or (not (list? x)) (not (list? y)))
    (list 'if '% x y)]
   ;If they are different length lists, they are obviously different.
   [(not (equal? (length x) (length y)))
    (list 'if '% x y)]
   ;If none of these simple checks are violated, we call exam-heads to examine them more thororoughly.
   [#t (exam-heads x y)]
))

;Taken from TA hint code.

(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
       (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))
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
