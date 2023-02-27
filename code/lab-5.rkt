;1 Square-pair
(define (square-pair n)
  (cons n (* n n)))

(square-pair 2)

;2 Rev
(define (rev p)
  (cons (cdr p) (car p)))

(rev (square-pair 2))

;3a Cartesian -> polar 
(define (c->p p)
  (define r (sqrt (+ (expt (car p) 2) (expt (cdr p) 2))))
  (define theta (atan (/ (cdr p) (car p))))
  (cons r theta))

(c->p (square-pair 2))

;3B Polar -> cartesian
(define (p->c p)
  (define x (* (car p) (cos (cdr p))))
  (define y (* (car p) (sin (cdr p))))
  (cons x y))

(p->c (c->p (square-pair 2)))

;4 Slope-intercept
(define (y p1 p2)
  (let* ((m (/ (- (cdr p2) (cdr p1))
               (- (car p2) (car p1))))
         (b (- (cdr p1) (* m (car p1)))))
    (lambda (x) (+ (* m x) b))))

