(define (make-tree value left right)
  (list value left right))

(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

;1a
(define (listmax l)
  (if (null? (cdr l))
      (car l)
      (max (car l) (listmax (cdr l)))))

;1b
(define (filter l k)
  (cond ((null? l) '())
        ((< (car l) k) (cons (car l)
                             (filter (cdr l) k)))
        ((>= (car l) k) (filter (cdr l) k))))

;1c
(define (treesize T)
  (if (null? T)
      0
      (+ 1 (treesize (left T))
         (treesize (right T)))))

;2a
(define (insert-heap v H)
  (if (null? H)
      (make-tree v '() '())
      (let ((child-value (max v (value H)))
            (root-value (min v (value H))))
        (make-tree root-value
                   (right H)
                   (insert-heap child-value (left H))))))

;2b
;The alternativing subtree heuristic ensures
;that when the trees are reconstructed, they are balanced.

;2c (Confusing)

;2d
(define (heapify l)
  (define (helper H l)
    (if (null? l)
        H
        (helper (insert-heap (car l) H) (cdr l))))
  (helper '() l))

;3a (Confusing) Why is the solution code including the pivot?

;3b (Confusing) Why is the solution code doing (- k (+ l 1)) and not (- k l 1)

;3c (Confusing)

;4a
(define (countlessthan T k)
  (cond ((null? T) 0)
        ((> (value T) k) (countlessthan (left T) k))
        ((= (value T) k) (+ 1 (countlessthan (left T) k)))
        ((< (value T) k) (+ 1 (countlessthan (left T) k) (countlessthan (right T) k)))))

;4b
(define (make-tree value left right totalsize)
  (list value left right totalsize))

(define (size T) (if (null? T) 0 (cadddr T)))

(define (insert T v)
  (cond ((null? T) (make-tree v '() '() 1))
        ((> v (value T)) (make-tree (value T) (left T) (insert (right T) v) (+ 1 (size T))))
        ((< v (value T)) (make-tree (value T) (insert (left T) v) (right T) (+ 1 (size T))))))

;4c
(define (countlessthan T k)
  (cond ((null? T) 0)
        ((>= (value T) k) (countlessthan (left T) k))
        ((< (value T) k) (+ (size (left T))
                            1
                            (countlessthan (right T) k)))))

(countlessthan (list 6 (list 4 (list -1 '() '() 1) (list 5 '() '() 1) 2) (list 16 '() (list 31 '() '() 1) 2) 6) 5)


          


  