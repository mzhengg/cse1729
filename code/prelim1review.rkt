;1a
(define (circle-area r)
  (define pi 3.14159)
  (* pi r r))

;1b
(define (S r)
  (define pi 3.14159)
  (* 4 (circle-area r)))

;1c
(define (v r)
  (* (/ r 3) (S r)))

;1d
(define (A r h)
  (define pi 3.14159)
  (* 2 pi r h))

;1e
(define (V r h)
  (define pi 3.14159)
  (* (/ (* pi h h) 3) (- (* 3 r) h)))

;2
(define (bakhshali-method S) 
  (define (an S x) (/ (- S (* x x)) (* 2 x)))
  (define (bakhshali-method-helper S x)
    (if (<= (abs (- (* x x) S)) 0.0001)
        x
        (bakhshali-method-helper S (- (+ x (an S x)) (/ (expt (an S x) 2) (* 2 (+ x (an S x))))))))
  (bakhshali-method-helper S (/ S 2)))

;3
(define (gregory x k)
  (define (posneg i) (expt -1 i))
  (define (body x i) (/ (expt x (+ (* 2 i) 1)) (+ (* 2 i) 1)))
  (define (gregory-helper x k i)
    (if (= i k)
        (* (posneg i) (body x i))
        (+ (* (posneg i) (body x i)) (gregory-helper x k (+ i 1)))))
  (gregory-helper x k 0))

;4
(define (gauss-legendre tol)
  (define (function-a a b) (/ (+ a b) 2))
  (define (function-b a b) (sqrt (* a b)))
  (define (function-t t p a1 a2) (- t (* p (expt (- a1 a2) 2))))
  (define (function-p p) (* 2 p))
  (define (approx-function a b t) (/ (expt (+ a b) 2) (* 4 t)))
  (define (gauss-legendre-helper a b t p)
    (if (< (- (function-a a b) (function-b a b)) tol)
        (approx-function a b t)
        (gauss-legendre-helper (function-a a b) (function-b a b) (function-t t p a (function-a a b)) (function-p p))))
  (gauss-legendre-helper 1 (/ 1 (sqrt 2)) (/ 1 4) 1))

(gauss-legendre 0.0001)

;5a
(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

;5b
(define (n-fold-smooth f dx n)
  (define 


  
