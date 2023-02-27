;1 New harmonic function
(define (inc x)
  (+ x 1))

(define (fraction x)
  (/ 1 (+ x 1)))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term (- a 1)) (sum term (next a) next b))))

(define (harmonic n)
  (sum fraction 1 inc n))

;2a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;2b
(define (inc x)
  (+ x 1))

(define (body x)
  (* (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))

(define (wallis-pi n)
  (product body 1 inc n))

;3
(define (frac-sum f g n)

  (define (inc i)
    (+ i 1))

  (define (main f g x)
    (if (= (g x) 0)
        0
        (/ (f x) (g x))))

  (define (sum term x next b)
    (cond ((> x b) 0)
          (else (+ (term f g x) (sum term (next x) next b)))))

  (if (= n 0)
      0
      (sum main (* -1 n) inc n)))

;4a
(define (der f h)
  (lambda (x) (/ (- (f (+ x h)) (f x)) h)))

;4b
(define (der-n f n h)
  (if (= n 1)
      (der f h)
      (der (der-n f (- n 1) h) h)))

;5a
(define (newton f x n)
   (if (= n 0)
      x
      (newton f (- x (/ (f x) ((der f .01) x))) (- n 1))))

;6a
(define (sum-term term a b)
  (if (> a b)
      0
      (+ (term a) (sum-term term (+ a 1) b))))

;6b
(define (simpson-integrate f a b n)
  (let ((dx (/ (- b a) n)))
    (define (simpson-integrate-help f a b n i)
      (cond ((= i 0) (+ (f (+ a (* i dx))) (simpson-integrate-help f a b n (+ i 1))))
            ((= i n) (f (+ a (* i dx))))
            ((= (modulo i 2) 1) (+ (* (f (+ a (* i dx))) 4) (simpson-integrate-help f a b n (+ i 1))))
            ((= (modulo i 2) 0) (+ (* (f (+ a (* i dx))) 2) (simpson-integrate-help f a b n (+ i 1))))))
    (* (/ dx 3) (simpson-integrate-help f a b n 0))))

(simpson-integrate (lambda (x) x) 0 1 2)
            
          
      
      
      



  

  

