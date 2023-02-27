(define (cur l) (car l))
(define (next l) (car (cdr l)))

(define (sort l)
  (define (sort-helper l defect-l)
      (cond ((null? (cdr l))
             (cons (cur l) defect-l))
            ((<= (cur l) (next l))
             (cons (cur l) (sort-helper (cdr l) defect-l)))
            ((> (cur l) (next l))
             (sort-helper (cdr l) (cons (cur l) defect-l)))))
  (sort-helper l (list)))

(define (sort-check l)
  (if (null? (cdr l))
      #t
      (if (> (cur l) (next l))
          #f
          (sort-check (cdr l)))))

(define (sort-final l)
  (if (sort-check l)
      l
      (sort-final (sort l))))

(define (list-sort l)
  (if (null? l)
      l
      (sort-final l)))
