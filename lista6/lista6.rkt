#lang plait
#|ZADANIE 1
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

Dla dowolnych funkcji f i g oraz listy xs zachodzi
(map f (map g xs)) ≡ (map (lambda (x) (f (g x))) xs)

i) P(empty)
   L ≡ (map f (map g null)) ≡ (map f null) ≡ null
   P ≡ (map (lambda (x) (f (g x))) null) ≡ null ≡ L
ii) Weźmy dowolne f, g, xs i załóżmy, że
(map f (map g xs)) ≡ (map (lambda (x) (f (g x))) xs).
Pokażmy, że (map f (map g (cons x xs)) ≡ (map (lambda (x) (f (g x))) (cons x xs)).

L ≡ (map f (map g (cons x xs))
  ≡ (map f (cons (g x) (map g xs)))
  ≡ (cons (f (g x)) (map f (map g xs))) ; zał ind
  ≡ (cons (f (g x)) (map (lambda (x) (f (g x))) xs))
  ≡ (map (lambda (y) (f (g y))) (cons x xs)) ≡ P
 
ZADANIE 2
(define (append xs ys)
  (if (empty? xs)
      ys
      (cons (first xs) (append (rest xs) ys))))

 pokaż, że dla dowolnych list xs i ys istnieje lista zs taka, że (append xs ys) ≡ zs.

i)
(append null ys) ≡ ys
ii)
Weźmy dowolne xs, ys i załóżmy, że istnieje lista zs taka, że (append xs ys) ≡ zs.
Pokażmy, że istnieje lista źs taka, że (append (cons x xs) ys) ≡ źs.
(append (cons x xs) ys) (append (cons x xs) ys)

ZADANIE 3
( define-type ( NNF 'v )
( nnf-lit [ polarity : Boolean ] [ var : 'v ])
( nnf-conj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ])
( nnf-disj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ]) )

; Zasada indukcji dla list:
; Niech P będzie własnością list, taką, że:
; i)  P(empty)
; ii) Dla każdej wartości x i każdej listy xs,
;     jeśli P(xs) to P((cons x xs))
; Wówczas dla dowolnej listy xs zachodzi P(xs)

Zasada indukcji dla NNF:
Niech P będzie własnością list, taką, że:
i) P zachodzi dla dowolnego literału
ii) Dla każdych formuł fi, psi (w NNF),
jeśli P(fi) i P(psi), to:
        P(nnf-conj fi psi) i P(nnf-disj fi psi)
Wówczas dla dowolnej formuły fi w NNF zachodzi P(fi)

ZADANIE 4 |#
(define-type ( NNF 'v )
( nnf-lit [ polarity : Boolean ] [ var : 'v ])
( nnf-conj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ])
( nnf-disj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ]) )


(define example (nnf-conj (nnf-lit #t 'a) (nnf-lit #t 'b)))
(define (Neg b) (if (equal? b #t) #f #t))

(define (neg-nnf fi)
  (type-case (NNF 'v) fi
    [(nnf-lit b var) (nnf-lit (Neg b) var)]
    [(nnf-conj fi psi) (nnf-disj (neg-nnf fi) (neg-nnf psi))]
    [(nnf-disj fi psi) (nnf-conj (neg-nnf fi) (neg-nnf psi))]))

; (neg-nnf example)
#|
(neg-nnf (neg-nnf φ)) ≡ φ

i)  φ jest literałem
      (neg-nnf (neg-nnf φ)) ≡ (neg-nnf (Neg φ)) ≡ (Neg (Neg φ))  ≡ φ
ii) Weźmy dowolne φ i ψ z NNF. Załóżmy, że (neg-nnf (neg-nnf φ)) ≡ φ i (neg-nnf (neg-nnf ψ)) ≡ ψ
      (neg-nnf (neg-nnf (nnf-conj φ ψ))) ≡ (neg-nnf (nnf-disj (neg-nnf φ) (neg-nnf ψ)))) ≡ (nnf-conj φ ψ)
      (neg-nnf (neg-nnf (nnf-disj φ ψ))) ≡ (neg-nnf (nnf-conj (neg-nnf φ) (neg-nnf ψ)))) ≡ (nnf-disj φ ψ)
|#

;ZADANIE 5

(define (eval-nnf [f :('a -> Boolean)] [φ : (NNF 'a)])
    (type-case (NNF 'a) φ
    [(nnf-lit b zmienna) (if b (f zmienna) (Neg (f zmienna)))]
    [(nnf-conj fi psi) (and (eval-nnf f fi) (eval-nnf f psi))]
    [(nnf-disj fi psi) (or (eval-nnf f fi) (eval-nnf f psi))]))
 
(define (examplef x)
  (if (or (equal? x 'a) (equal? x 'b)) #t #f))
#|
;(eval-nnf examplef example)
(eval-nnf σ (neg-nnf φ)) ≡ (not (eval-nnf σ φ))

 i) φ jest literałem
      (eval-nnf σ (neg-nnf φ)) ≡ (eval-nnf σ (Neg φ)) ≡ (not (eval-nnf σ φ))
ii) Weźmy dowolne φ i ψ z NNF. Załóżmy, że (eval-nnf σ (neg-nnf φ)) ≡ (not (eval-nnf σ φ)) i (eval-nnf σ (neg-nnf ψ)) ≡ (not (eval-nnf σ ψ))
      (eval-nnf σ (neg-nnf (nnf-conj φ ψ)))
    ≡ (eval-nnf σ (nnf-disj (neg-nnf φ) (neg-nnf ψ))))
    ≡ (or (eval-nnf σ (neg-nnf φ)) (eval-nnf σ (neg-nnf ψ)))
    ≡ (or (not (eval-nnf σ  φ)) (not (eval-nnf σ  ψ)))
    ≡ (not (and (eval-nnf σ  φ))  (eval-nnf σ  ψ))
    ≡ (not (eval-nnf σ (nnf-conj φ ψ)))
to samo dla alternatywy praktycznie

(define-type ( NNF 'v )
( nnf-lit [ polarity : Boolean ] [ var : 'v ])
( nnf-conj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ])
( nnf-disj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ]) )


|#

 ;ZADANIE 6

( define-type ( Formula 'v )
( var [ var : 'v ])
( neg [ f : ( Formula 'v ) ])
( conj [ l : ( Formula 'v ) ] [ r : ( Formula 'v ) ])
( disj [ l : ( Formula 'v ) ] [ r : ( Formula 'v ) ]) )

(define (to-nnf φ)
  (type-case (Formula 'v) φ
    [(var p) (nnf-lit #t p)]
    [(conj p q) (nnf-conj (to-nnf p) (to-nnf q))]
    [(disj p q) (nnf-disj (to-nnf p) (to-nnf q))]
    [(neg p)
     (type-case (Formula 'v) p
       [(neg w) (to-nnf w)]
       [(var p) (nnf-lit #f p)]
       [(conj p q) (nnf-disj (to-nnf (neg p)) (to-nnf (neg q)))]
       [(disj p q) (nnf-conj (to-nnf (neg p)) (to-nnf (neg q)))])]))

(define example2 (neg (conj (disj (var 'a) (neg (var 'b))) (var 'c))))
;(to-nnf example2)

;ZADANIE 7
(define (eval-formula [σ : ('a -> Boolean)] φ)
  (type-case (Formula 'v) φ
    [(var p) (σ p)]
    [(conj p q) (and (eval-formula σ p) (eval-formula σ q))]
    [(disj p q) (or (eval-formula σ p) (eval-formula σ q))]
    [(neg p) (not (eval-formula σ p))]))

;(eval-formula examplef example2)

#|
(eval-nnf σ (to-nnf φ)) ≡ (eval-formula σ φ)

 i) φ jest literałem
    (σ φ) ≡ (σ φ)
ii)  Weźmy dowolne φ i ψ. Załóżmy, że (eval-nnf σ (to-nnf φ)) ≡ (eval-formula σ φ) i (eval-nnf σ (to-nnf ψ)) ≡ (eval-formula σ ψ)
    (eval-nnf σ (to-nnf (conj φ ψ))
  ≡ (eval-nnf σ (nnf-conj (to-nnf φ) (to-nnf ψ))
  ≡ (and (eval-nnf σ (to-nnf φ)) (eval-nnf σ (to-nnf ψ)))
  ≡ (and (eval-formula σ φ) (eval-formula σ ψ))
  ≡ (eval-formula σ (conj φ ψ))
to samo dla alternatywy
|#

;ZADANIE 8
#| (sorted? xs) ≡ #t to (sorted? (insert x xs)) ≡ #t |#
(define (sorted? test< list)
  (or (empty? list)
      (empty? (rest list))
      (and (test< (first list) (first (rest list)))
           (sorted? test< (rest list)))))
;(sorted? < (list 1 2 3 4))

(define (insert-generic lt n xs)
  (if (empty? xs)
      (list n)
      (if (lt n (first xs))
          (cons n xs)
          (cons (first xs) (insert-generic lt n (rest xs))))))

;(insert-generic < 4 (list 1 3 5 9))
#|

 i) xs puste, (sorted? (insert x empty)) ≡ #t
ii) zakładam, że dla dowolnego x i dowolnego xs (sorted? xs) ≡ #t to (sorted? (insert x xs)) ≡ #t



|#