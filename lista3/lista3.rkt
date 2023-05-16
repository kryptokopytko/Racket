#lang racket
;ZADANIE 1
;'(( car ( a . b ) ) (* 2) )

;(list (cons 'car (list (cons 'a  'b))) (list '* 2))
;(list (list 'car (cons 'a 'b ) ) (list '* 2) )

;`( ,( car '( a . b ) ) ,(* 2) )

;(list 'a 2)
;(list ( car (cons 'a 'b ) ) (* 2) )

;'((+ 1 2 3) ( cons ) ( cons a b ) )

;(list (list '+ '1 '2 '3) (list 'cons) (list 'cons 'a 'b))
;(list (list '+ 1 2 3) (list 'cons ) (list 'cons 'a 'b ) )


;ZADANIE 2
(define (my-foldl f x xs)
  (define (it xs acc)
    (if (null? xs)
        acc
        (it (cdr xs) (f (car xs) acc))))
  (it xs x))

(define (product xs)
  (foldl * 1 xs))

;(product (list 5 4 3 2 1))

;ZADANIE 3
;(( lambda ( x y ) (+ x (* x y ) ) ) 1 2) ->
;(+ 1 (* 1 2) -> 3
;(( lambda ( x ) x ) ( lambda ( x ) x ) ) ->
;(( lambda (x) x)) -> #procedure
;(( lambda ( x ) x x ) ( lambda ( x ) x ) ) ->
;((lambda (x) x) -> #procedure
;(( lambda ( x ) x x ) ( lambda ( x ) x x ) ) ->
;((lambda (x) x x) -> #procedure

;ZADANIE 4

(define (my-compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))
(define (inc x)
  (+ x 1))

;(( my-compose square inc ) 5)
;(( my-compose inc square ) 5)

;ZADANIE 5
(define (negatives n)
  (build-list n (lambda (n) (- 0 n 1))))
;(negatives 5)

(define (reciprocals n)
  (build-list n (lambda (n) (/ 1 (+ 1 n)))))
;(reciprocals 6)

(define (evens n)
  (build-list n (lambda (n) (* 2 n))))
;(evens 3)

(define (identityM n)
  (define (it i acc)
    (if (= i -1)
        acc
        (it (- i 1) (cons
          (build-list n (lambda (k) (if (= i k) 1 0)))
          acc))))
  (it (- n 1) (list)))
;(identityM 12)

;ZADANIE 6
(define (empty-set x)
  #f)
(define (singleton x)
  (lambda (y) (equal? y x)))
(define (in a s)
  (s a))
(define (union s t)
  (lambda (x) (or (s x) (t x))))
(define (intersect s t)
  (lambda (x) (and (s x) (t x))))

;(in 9 (singleton 6))
;(in 8 (union (singleton 5) (lambda (k) (even? k))))

 
;ZADANIE 7
( define ( foldr-reverse xs )
   ( foldr ( lambda ( y ys ) ( append ys ( list y ) ) ) null xs ) )
;( length ( foldr-reverse ( build-list 10000 identity ) ) )
;consow: (n * (n+1))/2
;nieuzytkow (n * (n-1))/2

;ZADANIE 8
(define (list->llist xs)
  (lambda (ys) (append xs ys)))
(define (llist->list f) (f null))
(define (llist->null xs) xs)
(define (llist->singleton x) (lambda (ys) (cons x ys)))
(define (llist-append f g) (lambda (h) (f (g h))))
(define (foldr-llist-reverse xs)(llist->list (foldr (lambda (y ys) (llist-append ys (llist->singleton y))) llist->null xs)))

(define a (llist->null (list 100)))

(llist->list(list->llist (list 3 6 8)))




