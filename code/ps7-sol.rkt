;1
(define (ins x l)
  (if (null? l)
      (cons x l)
      (if (<= x (car l))
          (cons x l)
          (cons (car l) (ins x (cdr l))))))

;2
(define (insSort l)
  (define (helper l sortl)
    (if (null? l)
        sortl
        (helper (cdr l) (ins (car l) sortl))))
  (if (null? l)
      l
      (helper (cdr l) (list (car l)))))

;3a
(define (fold-right op initial sequence)
  (define (helper intl seq)
    (if (null? seq)
        intl
        (op (car seq) (helper intl (cdr seq)))))
  (helper initial sequence))

;3b
(define (fold-left op initial sequence)
  (define (helper intl seq)
    (if (null? seq)
        intl
        (op (car seq) (helper intl (cdr seq)))))
  (helper initial (reverse sequence)))

;3c
(define (my-map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))

;3d
(define (my-append seq1 seq2) (fold-right cons seq2 seq1))

;3e
(define (my-length sequence) (fold-right (lambda (x y) (+ 1 y)) 0 sequence))

;3f
(define (reverse-r sequence) (fold-right (lambda (x y) (append y (list x))) '() sequence))

;3g
(define (reverse-l sequence) (fold-right (lambda (x y) (append y (list x))) '() sequence))

;3h
(define (horner-eval x coefficient-list)
  (if (= x 0)
      (car coefficient-list)
      (/ (fold-right (lambda (a b) (* (+ a b) x)) 0 coefficient-list) x)))

;4ai
(define (prime? n)
  (define (helper n)
    (define (divisor? k) (= 0 (modulo n k)))
    (define (divisors-upto k)
      (and (> k 1)
           1
           (or (divisor? k) (divisors-upto (- k 1)))))
    (not (divisors-upto (- n 1))))
  (if (or (= n 1) (= n 0))
      #f
      (helper n)))

(define (left-truncatable-prime? p)
  (define (place n)
    (if (< n 1)
        -1
        (+ 1 (place (/ n 10)))))
  (define (helper n c)
    (if (and (< n 1) (= c (+ (place p) 1)))
        #t
        (if (prime? n)
            (helper (* (expt 10 (place n))
                       (- (/ n (expt 10 (place n)))
                          (floor (/ n (expt 10 (place n)))))) (+ c 1))
            #f)))
  (helper p 0))

;4aii
(define (find sequence test n)
  (define (find-helper sequence test n c i val)
    (if (= i n)
        c
        (if (test (sequence val))
            (find-helper sequence test n (+ c 1) (+ i 1) (+ val 1))
            (find-helper sequence test n (+ c 1) i (+ val 1)))))
  (find-helper sequence test n 0 0 1))

(define (nth-left-trunc-prime n)
  (find (lambda (x) x) left-truncatable-prime? n))

;4bi
(define (right-truncatable-prime? n)
  (if (< n 1)
      #t
      (if (prime? n)
          (right-truncatable-prime? (floor (/ n 10)))
          #f)))

;4bii
(define (nth-right-trunc-prime n)
  (find (lambda (x) x) right-truncatable-prime? n))

;4ci
(define (two-sided-prime? p)
  (if (and (left-truncatable-prime? p) (right-truncatable-prime? p))
      #t
      #f))

;4cii
(define (nth-two-sided-prime n)
  (find (lambda (x) x) two-sided-prime? n))

;5
(define (make-tree value left right)
  (list value left right))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

(define (tree-map T f)
  (if (null? T)
      '()
      (list (f (value T)) (tree-map (left T) f) (tree-map (right T) f))))

;6
(define (tree-equal? T1 T2)
  (cond ((and (null? T1) (null? T2)) #t)
        ((and (null? T1) (not (null? T2))) #f)
        ((and (null? T2) (not (null? T1))) #f)
        ((eq? (value T1) (value T2)) (and (tree-equal? (left T1) (left T2)) (tree-equal? (right T1) (right T2))))
        (else #f)))

;7a
(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((= x (value T)) T)
        ((> x (value T)) (make-tree (value T) (left T) (insert x (right T))))
        ((< x (value T)) (make-tree (value T) (insert x (left T)) (right T)))))

(define (insert-list L T)
  (if (null? L)
      T
      (insert-list (cdr L) (insert (car L) T))))

;7b
(define (sort-extract T)
  (if (null? T)
      '()
      (append (sort-extract (left T)) (list (value T)) (sort-extract (right T)))))

;7c
(define (tree-sort l)
  (sort-extract (insert-list l '())))





