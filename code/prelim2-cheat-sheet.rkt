;MESSING WITH CONS

;cons of two values, creates a pair of two values
(cons 1 2)

;cons of a value and a list, appends the value onto the list
(cons 1 '())
(cons 3 '(1 2))

;cons of a list and a value, creates a pair with a list as the first entry and the value as the second
(cons (list 1 2) 3)

;cons of a list and an empty list, creates a pair with the list as the first entry and nothing in the second entry
(cons (list 1 2 3) '())

;---------------------------
;MESSING WITH LISTS
;Recall that the structure of a list is (value (remaing elements))
;That is, a list has a pair structure, where the car of the list is the "value" and the cdr of the list is a "list of the remaining elements"

;CREATING LISTS
;First way
(list 1 2 3 4 5)

;Second way
'(1 2 3 4 5)

;IMPORTANT BUILT-IN LIST FUNCTIONS
;append combines lists together into one list
(append (list 1 2 3) (list 3 4 5) (list 6 7 8))
;appends can be nested
(append (list 1 2 3) (append (list 3 4 5) (list 6 7 8)))

;reverse reverses a list
(reverse (list 1 2 3))

;null checks if a list is empty
(null? '())
(null? (list 1 2))

;length outputs the size of a list
(length (list 1 2 3 4))

;IMPORTANT PRE-BUILT LIST FUNCTIONS
;function to find the max number in a list
(define (largest l)
  (if (null? (cdr l))
      (car l)
      (max (car l) (largest (cdr l)))))

;function to the the min number in a list
(define (smallest l)
  (if (null? (cdr l))
      (car l)
      (min (car l) (smallest (cdr l)))))

;function to remove a value x from the list
(define (remove x l)
  (if (= x (car l))
      (cdr l)
      (cons (car l) (remove x (cdr l)))))

;function that returns entries in a list less than or equal to a value k
(define (lessthan= k l)
  (cond ((null? l) '())
        ((<= (car l) k)
         (cons (car l) (lessthan= k (cdr l))))
        ((> (car l) k)
         (lessthan= k (cdr l)))))

;function that returns entries in a list greater than or equal to a value k
(define (greaterthan= k l)
  (cond ((null? l) '())
        ((>= (car l) k)
         (cons (car l) (greaterthan= k (cdr l))))
        ((< (car l) k)
         (greaterthan= k (cdr l)))))

;function to sort a list
(define (sort l)
  (if (null? l)
      '()
      (let ((smal-val (smallest l))
            (rem-list (remove (smallest l) l)))
        (cons smal-val (sort rem-list)))))

;another function to sort a list

;---------------------------
;MESSING WITH TREES
;Recall that fundamental programming does not allow structures to be altered.
;Anytime a function calls for a tree to be altered (either a value is removed, inserted, etc.), the entire tree must be reconstructed

;convenience functions
(define (make-tree value left right) (list value left right))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

;function for finding the size (number of nodes) of a binary tree
(define (tree-size T)
  (if (null? T)
      0
      (+ 1 (tree-size (left T)) (tree-size (right T)))))

;function for finding the depth (longest branch) of a binary tree
(define (tree-depth T)
  (if (null? T)
      -1
      (max (+ 1 (tree-depth (left T))) (+ 1 (tree-depth (right T))))))

;function for determining if an element is in a binary search tree
(define (element? x T)
  (cond ((null? T) #f)
        ((= x (value T)) #t)
        ((> x (value T)) (element? x (right T)))
        ((< x (value T)) (element? x (left T)))))

;function for inserting an element into a binary search tree
(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((= x (value T)) T)
        ((> x (value T)) (make-tree (value T) (left T) (insert x (right T))))
        ((< x (value T)) (make-tree (value T) (insert x (left T)) (right T)))))
;note that when inserting into a binary search tree we are simply trying to reach the end
;so that we can append it as a leaf

;function for finding the smallest element in a binary search tree
(define (smallest T)
  (if (null? (left T))
      (value T)
      (smallest (left T))))

;function for removing the smallest element in a binary search tree
(define (remove-smallest T)
  (if (and (null? (left T)) (null? (right T)))
      '()
      (make-tree (value T) (remove-smallest (left T)) (right T))))

;function for merging two binary search trees together
(define (merge-bst t1 t2)
  (make-tree (smallest t2)
             t1
             (remove-smallest t2)))

;function for removing a node from a binary search tree
(define (remove x T)
  (cond ((= x (value T)) (merge-bst (left T) (right T)))
        ((> x (value T)) (make-tree (value T) (left T) (remove x (right T))))
        ((< x (value T)) (make-tree (value T) (remove x (left T)) (right T)))))

;-------------------------
;MESSING WITH HEAPS

;convenience functions
(define (make-heap value left right)
  (list value left right))

(define (value H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))

;function for inserting a value into a heap
;an f with "<" yields a min-heap because at each node, we are picking the value that is smallest
;an f with ">" yields a max-heap because at each node, we are picking the value that is largest
(define (heap-insert f x H)
  (cond ((null? H) (make-heap x '() '()))
        ((f x (value H)) (make-heap x (right H) (heap-insert f (value H) (left H))))
        ((f (value H) x) (make-heap (value H) (heap-insert f x (right H)) (left H)))))
;note that when inserting into a heap, we are constantly comparing the current value to the value at the node
;and choosing the max or min depending on whether it is a min or max heap

(heap-insert < 9 (list 11 (list 18 (list 24 '() '()) '()) (list 12 (list 22 '() '()) (list 38 '() '()))))

;function for finding the smallest value in a heap
(define (smallest H)
  (cond ((and (null? (left H)) (null? (right H))) (value H))
        ((null? (left H)) (min (value H) (smallest (right H))))
        ((null? (right H)) (min (value H) (smallest (left H))))
        (else (min (value H) (smallest (left H)) (smallest (right H))))))

;function for finding the largest value in a heap
(define (largest H)
    (cond ((and (null? (left H)) (null? (right H))) (value H))
        ((null? (left H)) (max (value H) (largest (right H))))
        ((null? (right H)) (max (value H) (largest (left H))))
        (else (max (value H) (largest (left H)) (largest (right H))))))

;function for removing a value in a heap
(define (merge-heaps f h1 h2)
  (cond ((null? h1) h2)
        ((null? h2) h1)
        ((f (value h1) (value h2))
         (make-heap (value h1) h2 (merge-heaps f (left h1) (right h1))))
        ((f (value h2) (value h1))
         (make-heap (value h2) h1 ((merge-heaps f (left h2) (right h2)))))))















