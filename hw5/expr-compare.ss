#lang racket

(provide expr-compare)
(provide test-expr-compare)
(provide test-expr-x)
(provide test-expr-y)

(define LAMBDA (string->symbol "\u03BB"))
(define (combine x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
(define (comp x y) 
  (cond
  [(equal? x y) x]
  [(and (boolean? x) (boolean? y))
    (if x '% '(not %))]
  [else (list 'if '% x y)])
)
(define (comp-hash x y hashmapx hashmapy) (
  cond
    [(equal? (hash-ref hashmapx x x) (hash-ref hashmapy y y)) (hash-ref hashmapx x x)]
    [else (let ([xMap (hash-ref hashmapx x x)] [yMap (hash-ref hashmapy y y)])  (comp xMap yMap))]
  ))
(define (lambda-funct x y hashmapx hashmapy) 
  ;more or less where we use hash map
  (cond
    [(or (empty? x) (empty? y) (not (list? x)) (not (list? y)) (not (equal? (length x) (length y)))) (comp-hash x y hashmapx hashmapy)]
    [(and (or (hash-has-key? hashmapx 'lambda) (hash-has-key? hashmapx LAMBDA)
         (hash-has-key? hashmapy 'lambda) (hash-has-key? hashmapy LAMBDA)) 
         (or (equal? (car x) 'lambda) (equal? (car x) LAMBDA) (equal? (car y) 'lambda) (equal? (car y) LAMBDA))) 
        (cons (comp-hash (car x) (car y) hashmapx hashmapy) (lambda-funct (cdr x) (cdr y) hashmapx hashmapy))]
    [(or (equal? (car x) 'lambda) (equal? (car x) LAMBDA) (equal? (car y) 'lambda) (equal? (car y) LAMBDA))
     (lamb-check x y hashmapx hashmapy)]
     [else (cons (lambda-funct (car x) (car y) hashmapx hashmapy) (lambda-funct (cdr x) (cdr y) hashmapx hashmapy))]
  )
)
(define (lambda-parse-params x y hashmapx hashmapy) 
  (cond
    [(or (empty? x) (empty? y)) (comp x y)]
    [(not (equal? (hash-ref hashmapx (car x) (car x)) (hash-ref hashmapy (car y) (car y)))) (comp x y)]
    [else (cons (hash-ref hashmapx (car x) (car x)) (lambda-parse-params (cdr x) (cdr y) hashmapx hashmapy))]
  ))
(define (lambda-x-param x y hashmap)
  (cond
    [(or (empty? x) (empty? y)) hashmap]
    [(and (or (not (list? x)) (not (list? y))) (equal? x y)) hashmap]
    [(and (or (not (list? x)) (not (list? y))) (not (equal? x y))) 
      (hash-set hashmap x (combine x y))]
    [(equal? (car x) (car y)) (define hashG (hash-set hashmap (car x) (car x)))
      (lambda-x-param (cdr x) (cdr y) hashG)]
    [else (define hashG (hash-set hashmap (car x) (combine (car x) (car y))))
      (lambda-x-param (cdr x) (cdr y) hashG)]
  ))
(define (lambda-y-param x y hashmap)
  (cond
    [(or (empty? x) (empty? y)) hashmap]
    [(and (or (not (list? x)) (not (list? y))) (equal? x y)) hashmap]
    [(and (or (not (list? x)) (not (list? y))) (not (equal? x y))) 
      (hash-set hashmap y (combine x y))]
    [(equal? (car x) (car y)) (define hashG (hash-set hashmap (car y) (car y)))
      (lambda-y-param (cdr x) (cdr y) hashG)]
    [else (define hashG (hash-set hashmap (car y) (combine (car x) (car y))))
      (lambda-y-param (cdr x) (cdr y) hashG)]
  ))
(define (lamb-check x y hashmapx hashmapy) 
	(cond
    [(or (empty? (cdr x)) (empty? (cdr y))) (list (expr-compare (car x) (car y)))]
    [(or (not (or (equal? (car x) 'lambda) (equal? (car x) LAMBDA))) (not (or (equal? (car y) 'lambda) (equal? (car y) LAMBDA)))) (comp x y)]
		[(not (equal? (length (cdr x)) (length (cdr y)))) (comp x y)]
    [(not (equal? (length (cadr x)) (length (cadr y)))) (comp x y)]
    [else (if (or (equal? (car x) LAMBDA) (equal? (car y) LAMBDA))
      (cons LAMBDA (cons (lambda-parse-params (cadr x) (cadr y) (lambda-x-param (cadr x) (cadr y) hashmapx) (lambda-y-param (cadr x) (cadr y) hashmapy)) (lambda-funct (cddr x) (cddr y) (lambda-x-param (cadr x) (cadr y) hashmapx) (lambda-y-param (cadr x) (cadr y) hashmapy))))
      (cons 'lambda (cons (lambda-parse-params (cadr x) (cadr y) (lambda-x-param (cadr x) (cadr y) hashmapx) (lambda-y-param (cadr x) (cadr y) hashmapy)) (lambda-funct (cddr x) (cddr y) (lambda-x-param (cadr x) (cadr y) hashmapx) (lambda-y-param (cadr x) (cadr y) hashmapy)))))]
	)
)
(define (expr-compare x y) 
	;BEGIN TA HINT CODE
  (cond
	[(equal? x y) x]
	[(and (boolean? x) (boolean? y))
		(if x '% '(not %))]
	[(or (not (list? x))
	     (not (list? y)))
		(list 'if '% x y)]
	; END TA HINT CODE
	; need to add hash map tomfoolery with lambdas 
  [(or (empty? x) (empty? y)) (comp x y)]
  [(not (equal? (length x) (length y)) ) (list 'if '% x y)]
  [(or (equal? (car x) 'quote)  (equal? (car y) 'quote)) (list 'if '% x y)]
  [(or (equal? (car x) 'let)  (equal? (car y) 'let)) (list 'if '% x y)]
  [(not (equal? (equal? (car x) 'if)  (equal? (car y) 'if))) (list 'if '% x y)]
  [(or (equal? (car x) 'lambda) (equal? (car x) LAMBDA) (equal? (car y) 'lambda) (equal? (car y) LAMBDA))
    (define myHashx (hash-set (hash) 'INIT 'INIT))
    (define myHashy (hash-set (hash) 'INIT 'INIT))
     (lamb-check x y myHashx myHashy)]
	[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
    )
)

(define (test-expr-compare x y)
  (and (equal? (eval (list 'let '([% #t]) (expr-compare x y))) (eval x))
       (equal? (eval (list 'let '([% #f]) (expr-compare x y))) (eval y))))

(define test-expr-x '(if (equal? ((lambda (e z) (* (* z e) z)) 1 5) (/ (* 3 4) 2)) (+ 4 ((lambda (a b) (+ a b)) 1 2)) 89))
(define test-expr-y '(if (equal? ((lambda (a b) (* (* a b) b)) 3 4) (* (* 3 4) 4)) (+ 2 ((lambda (c e) (+ c e)) 1 2)) 12))



