;1
(define (change n denominations)
  (cond ((< n 0) 0)
        ((= n 0) 1)
        ((null? denominations) 0)
        (else (+ (change n (cdr denominations))
                 (change (- n (car denominations))
                         denominations)))))

;2
(define (make-change n den)
  (define (aux n den cur)
    (cond ((< n 0) '())
          ((= n 0) (list cur))
          ((null? den) '())
          (else (let ((result (aux n (cdr den) cur))
                      (tail (aux (- n (car den)) den (cons (car den) cur))))
                  (append result tail)))))
  (aux n den '()))

;3
(define (rle coins)
  (define (rle-aux coins nb last)
    (cond ((null? coins) (list (cons nb last)))
          ((= (car coins) last) (rle-aux (cdr coins) (+ nb 1) last))
          (else (cons (cons nb last) (rle-aux (cdr coins) 1 (car coins))))))
  (rle-aux (cdr coins) 1 (car coins)))

;4
(define (rle-all clist)
  (cond ((null? clist) '())
        (else (cons (rle (car clist)) (rle-all (cdr clist))))))

;5 Pearson
(define (list-sum elements)
  (if (null? elements)
      0
      (+ (car elements)
         (list-sum (cdr elements)))))

(define (average X) (/ (list-sum X) (length X)))

(average (list 1 2 3 4 5))

(define (var-map X)
  (define (square x) (* x x))
  (let ((mean (average X)))
    (map (lambda (x) (square (- x mean))) X)))

(define (stdev X)
  (sqrt (average (var-map X))))

(stdev (list 3 10 3 4 3))

(define (map2 f X Y)
  (if (null? X)
      '()
      (cons (f (car X) (car Y))
            (map2 f (cdr X) (cdr Y)))))

(map2 (lambda (x y) (* x y)) (list 2 2 2) (list 1 2 3))

(define (covar-elements X Y)
  (let ((meanX (average X))
        (meanY (average Y)))
    (map2 (lambda (x y)
            (* (- x meanX) (- y meanY)))
          X Y)))

(covar-elements (list 10 10 10 10 10) (list 10 10 5 1000 1))

(define (pearson X Y)
  (/ (average (covar-elements X Y))
     (* (stdev X) (stdev Y))))

(pearson (list 30 10 5 2) (list 2 5 10 30))

;; 6 fitting

(define (best-fit pX pY)
  (let* ((a (* (pearson pX pY) (stdev pY) (/ 1 (stdev pX))))
         (b (- (average pY) (* a (average pX))))) (cons a b)))

(best-fit (list 1 2 3) (list 4 5 6))

(define (best-fit-fn pX pY)
  (let* ((a (* (pearson pX pY) (stdev pY) (/ 1 (stdev pX))))
         (b (- (average pY) (* a (average pX)))))
    (lambda (x) (+ (* a x) b))))

((best-fit-fn (list 160 180 200 220 240 260 280) (list 126 103 82 75 78 40 20)) 1)

(define X (list 160 180 200 220 240 260 280))
(define Y (list 126 103 82 75 78 40 20))
(define fitline (best-fit-fn X Y))


;7-8 Mergesort
(define (merge la lb)
  (cond ((null? la) lb)
        ((null? lb) la)
        ((< (car la) (car lb)) (cons (car la) (merge (cdr la) lb)))
        (else (cons (car lb) (merge la (cdr lb))))))

(merge (list 1 3 5 9 10) (list 0 2 4 10 12))

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
        
