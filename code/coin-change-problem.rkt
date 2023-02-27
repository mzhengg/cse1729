;1
;This function returns the number of combinations of change by taking in a integer and a list of denominations
(define (change k l)
  (define (change-func n l)
    (cond ((null? l) 0)
          ((< n 0) 0)
          ((< n 0.01) 1)
          (else (+ (change-func (- n (car l)) l)
                   (change-func n (cdr l))))))
  (change-func k (reverse l)))

;2
;This function takes returns a list of all the other combinations as lists
(define (out l n)
  (define (helper l c append-l out-l)
    (cond ((null? l)
           (cons append-l out-l))
          ((= c n)
           (helper l 0 '() (cons append-l out-l)))
          (else
           (helper (cdr l) (+ c (car l)) (append (list (car l)) append-l) out-l))))
  (helper l 0 '() '()))

;This function returns one list with all different combinations
(define (make-change n den)
  (define (helper k den cur)
    (cond ((null? den) '())
          ((< k 0) '())
          ((< k 0.01) cur)
          (else (append (helper (- k (car den)) den (cons (car den) cur))
                   (helper k (cdr den) cur)))))
(out (helper n (reverse den) '()) n))

;3
(define (rle coins)
  (define (rle-helper coins prev i c-val out-l)
    (cond ((null? coins)
           (cons (cons i c-val) out-l))
          ((not (= (car coins) prev))
           (rle-helper coins (car coins) 0 (car coins) (cons (cons i c-val) out-l)))
          (else
           (rle-helper (cdr coins) (car coins) (+ i 1) (car coins) out-l))))
  (rle-helper coins (car coins) 0 (car coins) (list)))

;4
(define (rle-all lcoins)
  (if (null? lcoins)
      '()
      (cons (rle (car lcoins)) (rle-all (cdr lcoins)))))

(rle-all (make-change 27 (list 1 5 10 25)))