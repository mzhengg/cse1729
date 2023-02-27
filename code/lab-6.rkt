;1a
(define (list-at l i)
  (define (helper l i c)
    (if (= c i)
        (car l)
        (helper (cdr l) i (+ c 1))))
  (helper l i 0))

;1b 
(define (list-length l)
  (define (length-help l c)
    (if (null? l)
        c
        (length-help (cdr l) (+ c 1))))
  (length-help l 0))

(define (next-val l)
  (car (cdr l)))

(define (list-sorter l)
  (define (list-sorter-help pre-val l n i)
    (cond ((and (not (= i n)) (>= (next-val l) pre-val))
           (cons pre-val (list-sorter-help (next-val l) (cdr l) n (+ i 1))))
          ((and (not (= i n)) (not (>= (next-val l) pre-val)))
           (cons (next-val l) (list-sorter-help pre-val (cdr l) n (+ i 1))))
          (else (cons pre-val '()))))
  (list-sorter-help (car l) l (- (list-length l) 1) 0))

(define (list-checker l)
  (if (null? (cdr l))
      #t
      (if (> (car l) (next-val l))
          #f
          (list-checker (cdr l)))))

(define (list-finalizer l)
  (if (list-checker l)
      l
      (list-finalizer (list-sorter l))))

(define (list-median l)
  (let ((lst (list-finalizer l)))
    (cond ((= (modulo (list-length lst) 2) 0)
           (/ (+ (list-at lst (/ (list-length lst) 2)) (list-at lst (- (/ (list-length lst) 2) 1))) 2))
          ((= (modulo (list-length lst) 2) 1)
           (list-at lst (floor (/ (list-length lst) 2)))))))

;2a
(define (explode x)
  (define (length-val x)
    (define (length-val-helper x c)
      (if (< x 1)
          c
          (length-val-helper (/ x 10) (+ c 1))))
    (length-val-helper x 0))
  (define (explode-helper x)
    (if (= x 0)
      '()
      (cons (floor (/ x (expt 10 (- (length-val x) 1)))) (explode-helper (* 1000 (- (/ x (expt 10 (- (length-val x) 1))) (floor (/ x (expt 10 (- (length-val x) 1))))))))))
  (if (= x 0)
      (list 0)
      (explode-helper x)))

;2b
(define (implode l)
  (define (implode-helper l n)
    (if (= n 0)
        (car l)
        (+ (* (car l) (expt 10 n)) (implode-helper (cdr l) (- n 1)))))
  (implode-helper l (- (list-length l) 1)))

;2c
(define (has-property x)
  (let ((l (explode x))) 
    (define (helper-a l)
      (if (null? l)
          0
          (+ (car l) (helper-a (cdr l)))))
    (if (= (* (helper-a l) (implode (reverse (explode (helper-a l))))) x)
        #t
        #f)))
 
;2d
(define (find sequence test n)
  (define (find-helper sequence test n c i val)
    (if (= i n)
        c
        (if (test (sequence val))
            (find-helper sequence test n (+ c 1) (+ i 1) (+ val 1))
            (find-helper sequence test n (+ c 1) i (+ val 1)))))
  (find-helper sequence test n 0 0 1))

(find (lambda (x) x) has-property 2)

;2e
(define (fujiwara n)
  (find (lambda (x) x) has-property n))
  