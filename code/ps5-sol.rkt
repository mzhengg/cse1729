;1a
(define (encode p)
  (let ((x (car p))
        (y (cdr p)))
    (cond ((not (= x (max x y)))
           (+ x (* y y )))
          ((= x (max x y))
           (+ (* x x) x y)))))

;1b
(define (decode z)
  (cond ((< (- z (expt (floor (sqrt z)) 2)) (floor (sqrt z)))
        (cons (- z (expt (floor (sqrt z)) 2)) (floor (sqrt z))))
        ((>= (- z (expt (floor (sqrt z)) 2)) (floor (sqrt z)))
        (cons (floor (sqrt z)) (- z (expt (floor (sqrt z)) 2) (floor (sqrt z)))))))

;2a
(define (sub-complex c d)
  (cons (- (car c) (car d)) (- (cdr c) (cdr d))))

;2b
(define (div-complex c d)
  (let* ((a (car c))
        (b (cdr c))
        (c (car d))
        (d (cdr d))
        (denominator (+ (* c c) (* d d))))
    (cons (/ (+ (* a c) (* b d)) denominator) (/ (- (* b c) (* a d)) denominator))))

;3ai
(define (sum-quadratic-roots a b c)
  (define (make-complex real imag)
    (cons real imag))
  (sub-complex (make-complex 0 0) (div-complex b a)))


;3aii
(define (prod-quadratic-roots a b c)
  (div-complex c a))

;3bi
(define (sum-cubic-roots a b c d)
  (define (make-complex real imag)
    (cons real imag))
  (sub-complex (make-complex 0 0) (div-complex b a)))

;3bii
(define (sum-pairs-cubic-roots a b c d)
  (div-complex c a))

;3biii
(define (prod-cubic-roots a b c d)
  (define (make-complex real imag)
    (cons real imag))
  (sub-complex (make-complex 0 0) (div-complex d a)))
  
;4
(define (zip a b)
  (if (null? a)
      a
      (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

;5
(define (unzip pair-list)
  (define (helper-a pair-list)
    (if (null? pair-list)
        pair-list
        (cons (car (car pair-list)) (helper-a (cdr pair-list)))))
  (define (helper-b pair-list)
    (if (null? pair-list)
        pair-list
        (cons (cdr (car pair-list)) (helper-b (cdr pair-list)))))
  (cons (helper-a pair-list) (helper-b pair-list)))