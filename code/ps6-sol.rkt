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

;5a
(define (list-sum l)
  (if (null? l)
      0
      (+ (car l) (list-sum (cdr l)))))

;5b
(define (list-length l)
  (if (null? l)
      0
      (+ 1 (list-length (cdr l)))))

(define (average l)
  (/ (list-sum l) (list-length l)))


;5c
(define (var-map l)
  (let ((avg (average l)))
    (map (lambda (x) (expt (- x avg) 2)) l)))

;5d
(define (stdev l)
  (expt (* (/ 1 (list-length l)) (list-sum (var-map l))) (/ 1 2)))

;5e
(define (map2 f X Y)
  (if (and (null? X) (null? y))
    '()
    (cons (f (car X) (car Y)) (map2 f (cdr X) (cdr Y)))))

;5f
(define (covar-elements X Y)
  (define (covar-elements-helper l avg)
    (if (null? l)
        '()
        (cons (- (car l) avg) (covar-elements-helper (cdr l) avg))))
  (map2 (lambda (x y) (* x y)) (covar-elements-helper X (average X)) (covar-elements-helper Y (average Y))))

;5g
(define (pearson l1 l2)
  (/ (* (/ 1 (list-length l1)) (list-sum (covar-elements l1 l2)))
     (* (stdev l1) (stdev l2))))

;6a
(define (best-fit X Y)
  (let* ((a (* (pearson X Y) (/ (stdev Y) (stdev X))))
         (b (- (average Y) (* a (average X)))))
    (cons a b)))

;6b
(define (best-fit-fn pX pY)
  (let ((a (car (best-fit pX pY)))
        (b (cdr (best-fit pX pY))))
    (lambda (x) (+ (* a x) b))))

;7
(define (merge L1 L2)
  (define (merge-helper l1 l2 out-l)
    (cond ((and (null? l1) (null? l2)) out-l)
          ((null? l1) (merge-helper l1 '() (append out-l l2)))
          ((null? l2) (merge-helper '() l2 (append out-l l1)))
          ((< (car l1) (car l2)) (merge-helper (cdr l1) l2 (append out-l (list (car l1)))))
          ((= (car l1) (car l2)) (merge-helper (cdr l1) (cdr l2) (append out-l (list (car l1)) (list (car l2)))))
          ((> (car l1) (car l2)) (merge-helper l1 (cdr l2) (append out-l (list (car l2)))))))
  (merge-helper L1 L2 '()))

;8
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

(define (mergeSort l)
  (define (list-length l)
    (if (null? l)
        0
        (+ 1 (list-length (cdr l)))))
  (define (helper l1 l2 n i)
    (if (= i n)
        (cons l1 l2)
        (helper (cdr l1) (cons (car l1) l2) n (+ i 1))))
  (helper l '() (floor (/ (list-length l) 2)) 0)

  (merge (list-sort (car (helper l '() (floor (/ (list-length l) 2)) 0))) (list-sort (cdr (helper l '() (floor (/ (list-length l) 2)) 0)))))

(mergeSort '(1 6 7 5 7 8 7))