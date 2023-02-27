(define (make-tree value left right totalsize)
  (list value left right totalsize))

(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))
(define (size T) (if (null? T) 0 (cadddr T)))


(define (insert2 x T)
  (cond ((null? T) (make-tree x '() '() 1))
        ((< x (value T)) (make-tree (value T) (insert2 x (left T)) (right T) (+ 1 (size T))))
        ((> x (value T)) (make-tree (value T) (left T) (insert2 x (right T)) (+ 1 (size T))))))

;Note, a 1 is only being added to the nodes of the branches we are going down. This is because an element is being inserted
;at the end of that branch path. So, only those binary trees are growing in size by 1. Thus, there is no need for the other
;nodes to be increased by 1 because there is nothing being inserted at the end of those.

(define (insert v T)
  (cond ((null? T) (make-tree v '() '() 1))
        ((= v (value T)) T)
        ((< v (value T)) (make-tree (value T )
                                    (insert v (left T))
                                    (right T)
                                    (+ 1 (size T))))
        ((> v (value T)) (make-tree (value T)
                                    (left T)
                                    (insert v (right T))
                                    (+ 1 (size T))))))

(insert 5 (list 9 (list 7 '() (list 8 '() '() 1) 2) (list 27 (list 14 '() '() 1) (list 33 '() '() 1) 3) 5))

(define (countlessthan T k)
  (cond ((= (size T) 0) 0)
        ((and (= (size T) 1) (< (value T) k)) 1)
        ((and (= (size T) 1) (>= (value T) k)) 0)
        ((< (value T) k) (+ 1 (countlessthan (left T) k) (countlessthan (right T) k)))
        ((>= (value T) k) (countlessthan (left T) k))))

(countlessthan (list 9 (list 7 (list '() '() '() 0) (list 8 (list '() '() '() 0) (list '() '() '() 0) 1) 2) (list 27 (list 14 (list '() '() '() 0) (list '() '() '() 0) 1) (list 33 (list '() '() '() 0) (list '() '() '() 0) 1) 3) 5) 20)


