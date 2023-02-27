;1a
(define (encode p)
  (let ((x (car p))
        (y (cdr p)))
    (if (= x (max x y))
        (+ x y (* x x))
        (+ x (* y y)))))

;1b
(define (decode z)
  (let ((a (inexact->exact (- z (expt (floor (sqrt z)) 2))))
        (b (inexact->exact (expt (floor (sqrt z)) 2))))
    (if (< a b)
        (cons a b)
        (cons b (- a b)))))

;2a
(define (make-complex real imag) (cons real imag))
(define (real p) (car p))
(define (imag p) (cdr p))

(define (add-complex c d)
  (make-complex (+ (real c) (real d)) (+ (imag c) (imag d))))

(define (sub-complex c d)
  (make-complex (- (real c) (real d)) (- (imag c) (imag d))))

(define (mult-complex c d)
  (make-complex (- (* (real c) (real d))
                   (* (imag c) (imag d)))
                (+ (* (real c) (imag d))
                   (* (imag c) (real d)))))

(define (div-complex c d)
  (let ((denom (+ (* (real d) (real d)) (* (imag d) (imag d)))))
    (make-complex (/ (+ (* (real c) (real d))
                        (* (imag c) (imag d)))
                     denom)
                  (/ (- (* (imag c) (real d))
                        (* (real c) (imag d)))
                     denom))))

;3ai
(define (sum-quadratic-roots a b c)
  (sub-complex (make-complex 0 0) (div-complex b a)))

;3aii
(define (prod-quadratic-roots a b c)
  (div-complex c a))

;3bi
(define (sum-cubic-roots a b c d)
  (sub-complex (make-complex 0 0) (div-complex b a)))

;3bii
(define (sum-pairs-cubic-roots a b c d)
  (div-complex c a))

;3biii
(define (prod-cubic-roots a b c d)
  (sub-complex (make-complex 0 0) (div-complex d a)))

;4
(define (zip l1 l2)
  (if (null? l1)
      '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

;5
(define (unzip l)
  (define (helper-a l)
    (if (null? l)
        '()
        (cons (car (car l)) (helper-a (cdr l)))))
  (define (helper-b l)
    (if (null? l)
        '()
        (cons (cdr (car l)) (helper-b (cdr l)))))
  (cons (helper-a l) (helper-b l)))


      









  
        








