;convenience functions
(define (make-tree value left right)
  (list value left right))

(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

(define tree1 (list 22 (list 11 '() '()) (list 36 (list 28 (list 23 '() '()) '()) (list 54 '() '()))))
(define tree2 (list 30 (list 21 (list 4 '() '()) (list 25 '() '())) (list 31 '() (list 78 '() '()))))

(define (smallest T)
  (define (helper tree val)
    (if (null? tree)
        val
        (helper (left tree) (value tree))))
  (helper T '()))

(define (largest T)
  (define (helper tree val)
    (if (null? tree)
        val
        (helper (right tree) (value tree))))
  (helper T '()))

(define (size T)
  (if (null? T)
      0
      (+ 1 (size (left T)) (size (right T)))))

(define (depth T)
  (define (helper tree i)
    (if (null? tree)
        -1
        (max i (helper (left tree) (+ i 1)) (helper (right tree) (+ i 1)))))
  (max (helper T 0)))

(depth tree1)

(define (element? x T)
  (cond ((null? T) #f)
        ((= x (value T)) #t)
        ((> x (value T)) (element? x (right T)))
        ((< x (value T)) (element? x (left T)))))

(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((= x (value T)) T)
        ((> x (value T)) (make-tree (value T) (left T) (insert x (right T))))
        ((< x (value T)) (make-tree (value T) (insert x (left T)) (right T)))))

(define (remove x T)
  (cond ((and (= x (value T)) (null? (left T)) (null? (right T)))
         '())
        ((and (= x (value T)) (null? (left T)))
         (right T))
        ((and (= x (value T)) (null? (right T)))
         (left T))
        ((= x (value T))
         (make-tree (largest (left T))
                    (remove (largest (left T)) (left T))
                    (right T)))
        ((> x (value T)) (make-tree (value T) (left T) (remove x (right T))))
        ((< x (value T)) (make-tree (value T) (remove x (left T)) (right T)))))

(define (merge T1 T2)
  (if (null? T2)
      T1
      (merge (insert (value T2) T1)
             (remove (value T2) T2))))