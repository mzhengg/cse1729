(define (even n)
  (if (= (modulo n 2) 0)
      #t
      #f))

(define pi (expt (/ 2143 22) (/ 1 4)))

(define (f n)
  (/ (- (expt (+ 1 (sqrt 5)) n)
        (expt (- 1 (sqrt 5)) n))
     (* (expt 2 n) (sqrt 5))))

(define (dfact n)
  (define (odd n i)
    (if (= i (/ (+ n 1) 2))
        (- (* 2 i) 1)
        (* (- (* 2 i) 1) (odd n (+ i 1)))))
  (define (even n i)
    (if (= i (/ n 2))
        (* 2 i)
        (* (* 2 i) (even n (+ i 1)))))
  (if (= (modulo n 2) 0)
      (even n 1)
      (odd n 1)))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (pi-approx n)
  (define (body i) (/ (fact i) (dfact (+ (* 2 i) 1))))
  (define (pi-approx-helper n i)
    (if (> i n)
        0
        (+ (body i) (pi-approx-helper n (+ i 1)))))
  (* 2 (exact->inexact (pi-approx-helper n 0))))

(define (piecewise x)
  (cond ((< x -2) (- (expt x 2) 4))
        ((and (>= x -2) (<= x 2)) (+ (* -1 (expt x 2)) 4))
        ((> x 2) (- (expt x 2) 4))))

(define (gregory x k)
  (define (posneg i) (expt -1 i))
  (define (body x i) (/ (expt x (+ (* 2 i) 1)) (+ (* 2 i) 1)))
    (define (gregory-helper x k i)
      (if (> i k)
          0
          (+ (* (posneg i) (body x i)) (gregory-helper x k (+ i 1)))))
  (gregory-helper x k 0))

(define (splice f g)
  (lambda (x) (cond ((< x 0) (f x))
                    ((and (>= x 0) (<= x 1)) (+ (* (- 1 (- (* 3 (expt x 2)) (* 2 (expt x 3)))) (f x)) (* (- (* 3 (expt x 2)) (* 2 (expt x 3))) (g x))))
                    ((> x 1) (g x)))))

((splice (lambda (x) x) (lambda (x) (* x x))) 0)
      

