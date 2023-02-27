(define (change k l)
  (cond ((null? l) 0)
        ((= k 0) 1)
        ((< k 0) 0)
        (else (+ (change (- k (car l)) l)
                 (change k (cdr l))))))

(define (make-change n den)
  (define (helper n den cur)
    (cond ((null? den) '())
          ((= n 0) (cons cur '()))
          ((< n 0) '())
          (else (append (helper (- n (car den)) den (cons (car den) cur))
                        (helper n (cdr den) cur)))))
  (helper n den '()))

(define (rle coins)
  (define (helper prev i l out-l)
    (cond ((null? l) (cons (cons i prev) out-l))
          ((not (= prev (car l)))
           (helper (car l) 1 (cdr l) (cons (cons i prev) out-l)))
          (else
           (helper (car l) (+ i 1) (cdr l) out-l))))
  (reverse (helper (car coins) 1 (cdr coins) '())))

(define (rle-all lcoins)
  (if (null? lcoins)
      '()
      (cons (rle (car lcoins)) (rle-all (cdr lcoins)))))

(define (list-sum l)
  (if (null? l)
      0
      (+ (car l) (list-sum (cdr l)))))

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

(define (average l)
  (/ (list-sum l) (len l)))

(define (var-map l)
  (map (lambda (x) (expt (- x (average l)) 2)) l))

(define (stdev l)
  (sqrt (* (/ 1 (len l))
           (list-sum (var-map l)))))

(define (map2 f X Y)
  (if (or (null? X) (null? Y))
      '()
      (cons (f (car X) (car Y)) (map2 f (cdr X) (cdr Y)))))

(define (covar-elements X Y)
  (let ((meanX (average X))
        (meanY (average Y)))
  (map2 (lambda (x y) (* x y))
        (map (lambda (x) (- x meanX)) X)
        (map (lambda (y) (- y meanY)) Y))))

(define (pearson X Y)
  (/ (* (/ 1 (len X))
        (list-sum (covar-elements X Y)))
     (* (stdev X) (stdev Y))))

(define (best-fit X Y)
  (let* ((a (* (pearson X Y)
               (/ (stdev Y) (stdev X))))
         (b (- (average Y)
               (* a (average X)))))
    (cons a b)))

(define (best-fit-fn pX pY)
  (let ((a (car (best-fit pX pY)))
        (b (cdr (best-fit pX pY))))
    (lambda (x) (+ (* a x) b))))

(define (remove x l)
  (cond ((null? l) l)
        ((= x (car l)) (cdr l))
        (else (cons (car l) (remove x (cdr l))))))

(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((<= (car l1) (car l2))
         (cons (car l1) (merge (remove (car l1) l1) l2)))
        ((<= (car l2) (car l1))
         (cons (car l2) (merge l1 (remove (car l2) l2))))))

;No idea [figure out]
(define (mergeSort lv)
  (define (split lv left right)
    (cond ((null? lv) (cons left right))
          (else (split (cdr lv) (cons (car lv) right) left))))
  (cond ((null? lv) '())
        ((null? (cdr lv)) lv)
        (else (let* ((part (split lv '() '()))
                     (left (mergeSort (car part)))
                     (right (mergeSort (cdr part))))
                (merge left right)))))



























                
                     

