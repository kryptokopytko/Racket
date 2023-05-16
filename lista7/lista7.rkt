;ZADANIE 1
(define-type (2-3tree 'v )
  (leaf)
  (node2 [l : (2-3tree 'v)] [elem : 'v] [r : (2-3tree 'v)])
  (node3 [l : (2-3tree 'v)] [elem1 : 'v] [s : (2-3tree 'v)] [elem2 : 'v] [r : (2-3tree 'v)]))

;                     drzewo           funkcja porownujaca    
(define (2-3Tree? [t : (2-3tree 'v)] [f : ('v 'v -> Boolean)] min max)
  (type-case (2-3tree 'v) t
    [(leaf) (pair 0 #t)]
    [(node2 left elem right)
     (let ([result-l (2-3Tree? left  f min elem)])
     (let ([result-r (2-3Tree? right f elem max)])
     (if (not (= (fst result-l) (fst result-r))) (pair -1 #f)
         (if (and (snd result-l) (snd result-r))
             (if (and (f min elem) (f elem max))
                 (pair (+ 1 (fst result-l)) #t)
                 (pair -1 #f))
             (pair -1 #f)))))]
    [(node3 l eleml s elemr r)
     (let ([result-l (2-3Tree? l f min eleml)])
     (let ([result-s (2-3Tree? s f eleml elemr)])
     (let ([result-r (2-3Tree? r f elemr max)])
     (if (not (and (snd result-r) (snd result-s) (snd result-l)))
         (pair -1 #f)
         (if (not (and (= (fst result-l) (fst result-s)) (= (fst result-s) (fst result-r))))
             (pair -1 #f)
             (if (and (f min eleml) (f eleml elemr) (f elemr max))
                 (pair (+ 1 (fst result-l)) #t)
                 (pair -1 #f)))))))]))

(define (zadanie1 [t : (2-3tree 'v)] [f : ('v 'v -> Boolean)] min max)
  (snd (2-3Tree? t f min max)))


(define example-tree (node2 (node2
                             (node2 (leaf) 1 (leaf))
                                     4 (leaf))
                             5 (node3 (leaf) 6 (leaf) 9 (leaf))))
(define example-tree2 (node2 (node2
                              (leaf) 1 (leaf))
                             5 (node3 (leaf) 6 (leaf) 9 (leaf))))

(2-3Tree? example-tree2 < -inf.0 +inf.0 )





;ZADANIE 3
(define/contract (suffixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (match xs
    ['() (list null)]
    [(cons x xs) (cons (cons x xs) (suffixes xs))]))

;(suffixes (list 1 2 3 4))

;ZADANIE 4

(define/contract (sublists xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
   (if (null? xs)
       (list null)
       (append-map
        (λ (ys) (list (cons (car xs) ys) ys))
        (sublists (cdr xs)))))
(sublists (list 1 2 3 4))


;ZADANIE 6

(define/contract ( foldl-map f a xs )
  (parametric->/c [a b c] (-> (-> a b (cons/c c b)) b (listof a) (cons/c (listof c) b)))
   ( define ( it a xs ys )
      ( if ( null? xs )
           ( cons ( reverse ys ) a )
           ( let [( p ( f (car xs) a) ) ]
              ( it ( cdr p )
                   ( cdr xs )
                   ( cons ( car p ) ys ) ) ) ) )
   ( it a xs null ))

;( foldl-map ( lambda ( x a ) ( cons a (+ a x ) ) ) 0 '(1 2 3) )