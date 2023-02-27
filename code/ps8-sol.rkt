(define (make-tree val left right) (list val left right))

(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

;1
(define (arithvalue T)
  (cond ((and (null? (left T)) (null? (right T)))
         (value T))
        ((eq? (value T) #\+)
         (+ (arithvalue (left T)) (arithvalue (right T))))
        ((eq? (value T) #\*)
         (* (arithvalue (left T)) (arithvalue (right T))))
        ((eq? (value T) #\/)
         (/ 1 (arithvalue (left T))))
        ((eq? (value T) #\-)
         (* -1 (arithvalue (left T))))))

;2
(define (find-largest T)
  (define (helper T val)
    (if (null? T)
        val
        (helper (right T) (value T))))
  (helper T '()))

(define (delete-value v T)
  (cond ((null? T) '())
        ((< v (value T))
         (make-tree (value T) (delete-value v (left T)) (right T)))
        ((> v (value T))
         (make-tree (value T) (left T) (delete-value v (right T))))
        ((and (= v (value T)) (or (not (null? (left T))) (not (null? (right T)))))
         (make-tree (find-largest (left T)) (delete-value (find-largest (left T)) (left T)) (right T)))
        (else '())))

(define tree1 (list 10 (list 9 '() '()) (list 30 (list 27 '() '()) (list 37 '() '()))))

(delete-value 9 tree1)
;3
(define (insert x H)
  (if (null? H)
      (make-tree x '() '())
      (let ((child (max x (value H)))
            (root (min x (value H))))
        (make-tree root (right H) (insert child (left H))))))
  
(define (make-heap elements)
  (define (helper l H)
    (if (null? l)
        H
        (helper (cdr l) (insert (car l) H))))
  (helper elements '()))

(define (merge-heaps H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
        ((< (value H1) (value H2))
         (make-tree (value H1) H2 (merge-heaps (left H1) (right H1))))
        ((> (value H1) (value H2))
         (make-tree (value H2) H1 (merge-heaps (left H2) (right H2))))))
  
(define (remove-min x H)
  (cond ((null? H) H)
        ((= x (value H)) (merge-heaps (left H) (right H)))
        (else (make-tree (value H) (remove-min x (left H)) (remove-min x (right H))))))

(define (extract-min H)
  (cond ((and (null? (left H)) (null? (right H)))
         (value H))
        ((and (null? (left H)) (not (null? (right H))))
         (min (value H) (extract-min (right H))))
        ((and (not (null? (left H))) (null? (right H)))
         (min (value H) (extract-min (left H))))
        (else (min (value H) (extract-min (left H)) (extract-min (right H))))))

(define (hsort elements)
  (define (helper H)
    (if (null? H)
        '()
        (cons (extract-min H) (helper (remove-min (extract-min H) H)))))
  (helper (make-heap elements)))

;4a
(define (heap-insert f x H)
  (cond ((null? H) (make-tree x '() '()))
        ((f x (value H)) (make-tree x (right H) (heap-insert f (value H) (left H))))
        ((f (value H) x) (make-tree (value H) (right H) (heap-insert f x (left H))))))

(define (combine f Ha Hb)
  (cond ((null? Ha) Hb)
        ((null? Hb) Ha)
        ((f (value Ha) (value Hb))
         (make-tree (value Ha) Hb (combine f (left Ha) (right Ha))))
        ((f (value Hb) (value Ha))
         (make-tree (value Hb) Ha (combine f (left Hb) (right Hb))))))

(define (remove f x H)
  (cond ((null? H) H)
        ((= x (value H)) (combine f (left H) (right H)))
        (else (make-tree (value H) (remove f x (left H)) (remove f x (right H))))))

(define (equalize-heaps heap-pair)
  (let ((length-H1 (car (car heap-pair)))
        (H1 (cdr (car heap-pair)))
        (length-H2 (car (cdr heap-pair)))
        (H2 (cdr (cdr heap-pair))))
    (cond ((> (- length-H1 length-H2) 1)
           (equalize-heaps (cons (cons (- length-H1 1)
                                       (remove >= (value H1) H1))
                                 (cons (+ length-H2 1)
                                       (heap-insert < (value H1) H2)))))
          ((> (- length-H2 length-H1) 1)
           (equalize-heaps (cons (cons (+ length-H1 1)
                                       (heap-insert > (value H2) H1))
                                 (cons (- length-H2 1)
                                       (remove <= (value H2) H2)))))
          (else
           (cons (cons length-H1 H1) (cons length-H2 H2))))))

;4b
(define (add-number x heap-pair)
  (let ((length-H1 (car (car heap-pair)))
        (H1 (cdr (car heap-pair)))
        (length-H2 (car (cdr heap-pair)))
        (H2 (cdr (cdr heap-pair))))
    (if (< x (value H1))
        (equalize-heaps (cons (cons (+ length-H1 1) (heap-insert > x H1))
                              (cons length-H2 H2)))
        (equalize-heaps (cons (cons length-H1 H1)
                              (cons (+ length-H2 1) (heap-insert < x H2)))))))

;4c
(define (get-median heap-pair)
  (let ((length-H1 (car (car heap-pair)))
        (H1 (cdr (car heap-pair)))
        (length-H2 (car (cdr heap-pair)))
        (h2 (cdr (cdr heap-pair))))
    (cond ((= length-H1 length-H2)
           (/ (+ (value H1) (value H2)) 2))
          ((> length-H1 length-H2)
           (value H1))
          ((> length-H2 length-H1)
           (value H2)))))








