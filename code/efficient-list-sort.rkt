;1st way
(define (smallest l)
  (define (smaller a b) (if (< a b) a b))
  (if (null? (cdr l))
      (car l)
      (smaller (car l) (smallest (cdr l)))))

(define (remove v elements)
  (if (null? elements)
      elements
      (if (equal? v (car elements))
          (cdr elements)
          (cons (car elements) (remove v (cdr elements))))))

(define (selSort l)
  (if (null? l)
      '()
      (let* ((first (smallest l))
              (rest (remove first l)))
        (cons first (selSort rest)))))

(selSort (list 2 5 1 45 5 7))


;2nd way
(define (make-pair a b) (cons a b))
(define (first p) (car p))
(define (second p) (cdr p))

(define (extractSmallest l)
  (if (null? (cdr l))
      (make-pair (car l) '())
      (let ((p (extractSmallest (cdr l))))
        (if (< (car l) (first p))
            (make-pair (car l) (cons (first p) (second p)))
            (make-pair (first p) (cons (car l) (second p)))
            ))))

(extractSmallest (list 1 2 5 6 3 2 10))