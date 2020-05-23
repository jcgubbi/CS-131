#lang racket

(define LAMBDA (string->symbol "\u03BB"))
(define (combine x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
(define (comp x y) 
  (cond
  [(equal? x y) x]
  [(and (boolean? x) (boolean? y))
    (if x '% '(not %))]
  [else (list 'if '% x y)])
)
(define (comp-hash x y hashmap) (
  cond
    [(equal? x y) (hash-ref hashmap x x)]
    [else (let ([xMap (hash-ref hashmap x x)] [yMap (hash-ref hashmap y y)])  (comp xMap yMap))]
))

(define (lambda-funct x y hashmap) 
  ;more or less where we use hash map
  (cond
    [(or (empty? x) (empty? y) (not (list? x)) (not (list? y)) (not (equal? (length x) (length y)))) (comp-hash x y hashmap)]
    [else (cons (lambda-funct (car x) (car y) hashmap) (lambda-funct (cdr x) (cdr y) hashmap))]
  )
)
(define (lambda-parse-params x y hashmap) 
  (cond
    [(or (empty? x) (empty? y)) (comp x y)]
    [else (cons (hash-ref hashmap (car x) (car x)) (lambda-parse-params (cdr x) (cdr y) hashmap))]
  ))
(define (lambda-param x y hashmap) 
  ;more or less where we build hash map
  (cond
    [(or (empty? x) (empty? y)) hashmap]
    [(and (or (not (list? x)) (not (list? y))) (equal? x y)) hashmap]
    [(and (or (not (list? x)) (not (list? y))) (not (equal? x y))) 
    (define hashG (hash-set hashmap x (combine x y)))
      (hash-set hashG y (combine x y))]
    [(equal? (car x) (car y)) (define hashG (hash-set hashmap (car x) (car x)))
      (lambda-param (cdr x) (cdr y) hashG)]
    [else (define hashG (hash-set hashmap (car x) (combine (car x) (car y))))
      (define hashN (hash-set hashG (car y) (combine (car x) (car y))))
      (lambda-param (cdr x) (cdr y) hashN)]
  ))
(define (lamb-check x y hashmap) 
	(cond
    [(or (empty? (cdr x)) (empty? (cdr y))) (list (expr-compare (car x) (car y)))]
    [(or (not (or (equal? (car x) 'lambda) (equal? (car x) LAMBDA))) (not (or (equal? (car y) 'lambda) (equal? (car y) LAMBDA)))) (comp x y)]
		[(not (equal? (length (cdr x)) (length (cdr y)))) (comp x y)]
    [(not (equal? (length (cadr x)) (length (cadr y)))) (comp x y)]
    [else (if (or (equal? (car x) LAMBDA) (equal? (car y) LAMBDA))
      (cons LAMBDA (cons (lambda-parse-params (cadr x) (cadr y) (lambda-param (cadr x) (cadr y) hashmap)) (lambda-funct (cddr x) (cddr y) (lambda-param (cadr x) (cadr y) hashmap))))
      (cons 'lambda (cons (lambda-parse-params (cadr x) (cadr y) (lambda-param (cadr x) (cadr y) hashmap)) (lambda-funct (cddr x) (cddr y) (lambda-param (cadr x) (cadr y) hashmap)))))]
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
    (define myHash (hash-set (hash) 'INIT 'INIT))
     (lamb-check x y myHash)]
	[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
    )
)

;;brend tests
(display "1: ")
(if (equal? (expr-compare 12 20) '(if % 12 20)) "true" (expr-compare 12 20))
(display "2: ")
(if (equal? (expr-compare 12 12)    '12) "true" (expr-compare 12 12))
(display "3: ")
(if (equal? (expr-compare 12 20)   '(if % 12 20)) "true"  (expr-compare 12 20))
(display "4: ")
(if (equal? (expr-compare #t #t)    '#t) "true" (expr-compare #t #t) )
(display "5: ")
(if (equal? (expr-compare #f #f)    '#f) "true" (expr-compare #f #f))
(display "6: ")
(if (equal? (expr-compare #t #f)    '%) "true" (expr-compare #t #f))
(display "7: ")
(if (equal? (expr-compare #f #t)    '(not %)) "true" (expr-compare #f #t))
(display "8: ")
(if (equal? (expr-compare 'a '(cons a b))    '(if % a (cons a b))) "true" (expr-compare 'a '(cons a b)))
(display "9: ")
(if (equal? (expr-compare '(cons a b) '(cons a b))    '(cons a b)) "true" (expr-compare '(cons a b) '(cons a b)))
(display "10: ")
(if (equal? (expr-compare '(cons a lambda) '(cons a λ))    '(cons a (if % lambda λ))) "true" (expr-compare '(cons a lambda) '(cons a λ)))
(display "11: ")
(if (equal? (expr-compare '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c)))
   '(cons (cons a (if % b c)) (cons (if % b a) c))) "true" (expr-compare '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c))))
(display "12: ")
(if (equal? (expr-compare '(cons a b) '(list a b))  '((if % cons list) a b)) "true" (expr-compare '(cons a b) '(list a b)))
(display "13: ")
(if (equal? (expr-compare '(list) '(list a))    '(if % (list) (list a))) "true" (expr-compare '(list) '(list a)))
(display "14: ")
(if (equal? (expr-compare ''(a b) ''(a c))  '(if % '(a b) '(a c))) "true" (expr-compare ''(a b) ''(a c)))
(display "15: ")
(if (equal? (expr-compare '(quote (a b)) '(quote (a c)))    '(if % '(a b) '(a c))) "true" (expr-compare '(quote (a b)) '(quote (a c))))
(display "16: ")
(if (equal? (expr-compare '(quoth (a b)) '(quoth (a c)))    '(quoth (a (if % b c)))) "true" (expr-compare '(quoth (a b)) '(quoth (a c))))
(display "17: ")
(if (equal? (expr-compare '(if x y z) '(if x z z))    '(if x (if % y z) z)) "true" (expr-compare '(if x y z) '(if x z z)))
(display "18: ")
(if (equal? (expr-compare '(if x y z) '(g x y z))
	'(if % (if x y z) (g x y z))) "true" (expr-compare '(if x y z) '(g x y z)))
(display "19: ")
(if (equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) 
	'((lambda (a) ((if % f g) a)) (if % 1 2))) "true" (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)))
(display "20: ")
(if (equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
	'((λ (a) ((if % f g) a)) (if % 1 2))) "true" (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)))
(display "21: ")
(if (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
	'((lambda (a!b) a!b) (if % c d))) "true" (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)))
(display "22: ")
(if (equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) 
	'(if % '((λ (a) a) c) '((lambda (b) b) d))) "true" (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))	)
(display "23: ")
(if (equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
   '(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2))
   ) "true" (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2))))
(display "24: ")
(if (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
	'((λ (a b) (f (if % a b) (if % b a))) 1 2)) "true" (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2)))
(display "25: ")
(if (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
                          '((λ (a c) (f c a)) 1 2))
   '((λ (a b!c) (f (if % a b!c) (if % b!c a)))
    1 2)) "true" (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2)))
(display "26: ")
(if (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
	'((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ))) 3))) "true" (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3)))
(display "27: ")
(if (equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
   '((λ (a)
      ((if % eq? eqv?)
       a
       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
        a (λ (a!b) (if % a!b a)))))
     (lambda (b!a a!b) (b!a a!b)))
) "true" (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b)))))



; EXTRA TEST CASES
(expr-compare '(cons a lambda) '(cons a λ))
;    =>       '(cons a (if % lambda λ))
(expr-compare '(lambda (a) a) '(lambda (b) b))
;    =>       '(lambda (a!b) a!b)
(expr-compare '(lambda (a) b) '(cons (c) b))
;    =>       '(if % (lambda (a) b) (cons (c) b))
(expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
;    =>       '((λ (if!fi) (+ if!fi 1)) 3)
(expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
;    =>       '(λ (lambda!λ) lambda!λ)
(expr-compare ''lambda '(quote λ))
;    =>       '(if % 'lambda 'λ)
(expr-compare '(lambda (a b) a) '(λ (b) b))
;    =>       '(if % (lambda (a b) a) (λ (b) b))
(expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
;    =>       '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b)))
(expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
;    =>       '(λ (let) (let (((if % x y) 1)) (if % x y)))
(expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
;      ⇒      '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x)))
(expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
;      ⇒      '(((λ (g!x)
;                   ((λ (x!n) (g!x (λ () (x!n x!n))))
;                    (λ (x!r) (g!x (λ () (x!r x!r))))))
;                 (λ (r!g)
;                   (λ (n!x) (if (= n!x 0)
;                                1
;                                (* n!x ((r!g) (- n!x 1)))))))
;                (if % 10 9))

