#lang racket

(require rackunit)

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

(define (tree? x)
  (cond [(leaf? x) #t]
        [(node? x) (and (tree? (node-l x)) (tree? (node-r x)))]
        [else #f]))

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))

(define (find-bst x t)
  (cond [(leaf? t) #f]
        [(node? t)
         (cond [(= x (node-elem t)) #t]
               [(< x (node-elem t))
                (find-bst x (node-l t))]
               [else
                (find-bst x (node-r t))])]))


;ZADANIE 2
(define (tree-fold f x t)
  (cond [(leaf? t) x]
        [(node? t)
         (f (tree-fold f x (node-r t))
            (node-elem t)
            (tree-fold f x (node-l t)))]))

(define (tree-sum t)
  (tree-fold + 0 t))

(define (tree-flip t)
  (tree-fold node (leaf) t))

(define (tree-height t)
  (define (heightf x y z)
  (+ 1 (max x z)))
  (tree-fold heightf 0 t))

(define (flatten t)
  (define (f x y z)
    (append x (list y) z))
  (tree-fold f null t))

(define (tree-span t)
  (cons
   (tree-fold (λ(l e p) (if (null? p) e p)) null t)
   (tree-fold (λ(l e p) (if (null? l) e l))  null t)))

;(flatten example-tree)
;(tree-span example-tree)
;(tree-sum example-tree)
;(tree-flip example-tree)
;(tree-height example-tree)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;ZADANIE 4
             
(define flat (λ (t xs)
               (cond [(leaf? t) xs]
                     [(node? t)
                      (flat (node-l t)
                            (cons (node-elem t) (flat (node-r t) xs)))])))
(define (better-flatten t) (flat t null))
(better-flatten example-tree)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;ZADANIE 5

(define (new-insert-bst x t)
  (cond [(leaf? t) (node (leaf) x (leaf))]
        [(node? t)
         (cond ;[(= x (node-elem t)) t]
               [(> x (node-elem t))
                (node
                 (node-l t)
                 (node-elem t)
                 (new-insert-bst x (node-r t)))]
               [else
                (node
                 (new-insert-bst x (node-l t))
                 (node-elem t)
                 (node-r t))])]))


(define (list->bst xs)
  (define (it xs t)
    (if (null? xs) t
        (it (cdr xs) (new-insert-bst (car xs) t))))
  (it xs (leaf)))
 
;(list->bst (list 4 2 0 9 4))

(define (treesort xs)
  (flatten (list->bst xs)))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;ZADANIE 6

;drzewo z korzeniem do wyciecia

(define (new-root t) ; szukanie nowego korzenia
  (define (it min t)
    (if (leaf? t) min
        (it (node-elem t) (node-l t))))
  (it (node-elem t) (node-r t)))

(define (new-tree t)
  (define newroot (new-root t))
  (define (chopped-tree newroot t)   ; prawe poddrzewo bez nowego korzenia
    (if (= (node-elem t) newroot)
      (leaf)
      (node (chopped-tree newroot (node-l t)) (node-elem t) (node-r t))))
  (if (leaf? (node-r t))
      (node-l t)
      (node (node-l t) newroot (chopped-tree newroot (node-r t))))) ;drzewo bez starego korzenia

(define (delete x t)
      (cond [(> x (node-elem t))
             (node (node-l t) (node-elem t) (delete x (node-r t)))]
            [(< x (node-elem t))
             (node (delete x (node-l t)) (node-elem t) (node-r t))]
            [else
             (new-tree t)]))

;(delete 1 example-tree)


;ZADANIE 7


(define empty-queue (cons null  null))
(define (empty? q) (if (and (null? (car q)) (null? (cdr q))) #t #f))
(define (push-back x q)
  (if (empty? q)
      (cons (list x) null)
  (cons (car q) (cons x (cdr q)))))
(define (front q)
  (if (empty? q) #f
      (caar q)))
(define (pop q)
  (define (it q)
    (if (null? (cdr q))
        null
        (cons (car q) (it (cdr q)))))
  (define (first q)
    (if (null? (cdr q)) (car q) (first (cdr q))))
  (cons (list (first q)) (it (cdr q))))

(define example-queue (push-back 6 (push-back 5(push-back 4(push-back 3 empty-queue)))))
example-queue

(pop (pop example-queue))

;(front example-queue)

