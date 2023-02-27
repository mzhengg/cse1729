(define (make-tree value left right)
  (list value left right))

(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

;1a (returns the maximum number in a list)
(define (listmax l)
  (if (null? (cdr l))
      (car l)
      (max (car l) (listmax (cdr l)))))

;1b (returns all numbers of a list less than a given value k)
(define (filter l k)
  (cond ((null? l) '())
        ((>= (car l) k) (append '() (filter (cdr l) k)))
        ((< (car l) k) (append (list (car l)) (filter (cdr l) k)))))

;1c (determines the (size) number of nodes in a binary tree)
(define (tree-size T)
  (if (null? T)
      0
      (+ 1 (tree-size (left T)) (tree-size (right T)))))

;1d (determines the (depth) longest branch in a binary tree)
(define (tree-depth T)
  (if (null? T)
      -1
      (max (+ 1 (tree-depth (left T))) (+ 1 (tree-depth (right T))))))

;2a (inserts a value into a min-heap)
(define (minheap-insert x H)
  (if (null? H)
      (make-tree x '() '())
      (let ((root (min x (value H)))
            (child (max x (value H))))
        (make-tree root (right H) (minheap-insert x (left H))))))

;2b (inserts a value into a max or min heap: < indicates a min heap and > indicates a max heap)
(define (heap-insert f x H)
  (cond ((null? H) (make-heap x '() '()))
        ((f x (value H)) (make-heap x (right H) (heap-insert f (value H) (left H))))
        ((f (value H) x) (make-heap (value H) (right H) (heap-insert f x (left H))))))

;2c
;The purpose of the alternating subtree heuristic is to keep the tree balanced by keeping the depth to a minimum. Otherwise,
;if the subtrees are not alternated, repeated inserts of a value into a tree will result in one side of the tree to be
;longer than the other as the operation is only applied to one particular side. This results in an inefficient tree because
;the more recursive calls are needed in order to retrieve the values at the leafs.

;2d
;Inserting the element into the smaller of the two subtrees should yield the same result. By adding elements into the
;smallest subtree for each recursive call, we ensure that the trees will remain balanced with a maximum difference in size
;at any point in time to be +/- 1.

;2e
(define (heapify numbers)
  (define (helper l H)
    (if (null? l)
        H
        (helper (cdr l)
                (minheap-insert (car l) H))))
  (helper numbers '()))

;4a
(define (countlessthan k T)
  (cond ((null? T) 0)
        ((> (value T) k) (countlessthan k (left T)))
        ((<= (value T) k) (+ 1 (countlessthan k (left T)) (countlessthan k (right T))))))

;4b
(define (make-tree value left right totalsize)
  (list value left right totalsize))

(define (size T) (if (null? T) 0 (cadddr T)))

(define (insert v T)
  (define (helper v T i)
    (cond ((null? T) (make-tree v '() '() 1))
          ((> v (value T))
           (let ((root (min v (value T)))
                 (child (max v (value T))))
             (make-tree root (right T) (insert child (left T)) i))
           ((< v (value T))
            (let ((root (min v (value T))

            (make-tree (value T) (insert v (right T)) (left T)))
           ((= v (value T))







      







