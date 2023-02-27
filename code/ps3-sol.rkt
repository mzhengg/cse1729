;1a Harmonic Numbers
(define (harmonic n)
  (if (= n 1)
      1
      (+ (/ 1 n) (harmonic (- n 1)))))

;1b Euler Estimate
(define (Eulerest n) (abs (- (harmonic n) (log n))))

;2 Primality test
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (and (> n 1)
       (not (divisors-upto (- n 1)))))

; 2 Count Primes
(define (count-primes m)
  (define (count-primes-helper c m)
    (cond ((<= m 1) c)
          ((prime? m) (count-primes-helper (+ c 1) (- m 1)))
          ((not (prime? m)) (count-primes-helper c (- m 1)))))
  (count-primes-helper 0 m))

; 3 Relatively Prime
(define (rel-prime a b)
  (define (divides-both d)
    (and (= 0 (modulo a d))
         (= 0 (modulo b d))))
  (define (divisor-upto k)
    (and (> k 1)
         (or (divides-both k)
             (divisor-upto (- k 1)))))
  (not (divisor-upto (min a b))))

(define (counter-deducter n)
  (if (= n 1)
      0
      (+ (- n 1) (counter-deducter (- n 1)))))

(define (counter a b c)
  (cond ((= b 1) (+ c 1))
        ((rel-prime a b) (counter a (- b 1) (+ c 1)))
        ((not (rel-prime a b)) (counter a (- b 1) c))))

(define (count-rel-prime-helper a b)
  (if (= a 1)
      (counter 1 b 0)
      (+ (counter a b 0) (count-rel-prime-helper (- a 1) (- b 1)))))

(define (count-rel-prime n)
  (count-rel-prime-helper n n))
        
; 4a Lucas Numbers
(define (lucas n)
  (cond ((= n 0) 2)
        ((= n 1) 1)
        ((> n 1) (+ (lucas (- n 1)) (lucas (- n 2))))))

; 4b Lucas Number Ratios
(define (Lucas-ratio n)
  (/ (lucas n) (lucas (- n 1))))

(define (fibonacci n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (Fibonacci-ratio n)
  (/ (fibonacci n) (fibonacci (- n 1))))

; 4c
(define (fast-Lucas-help n k luc-a luc-b)
  (if (= n k)
      luc-a
      (fast-Lucas-help n (+ k 1) (+ luc-a luc-b) luc-a)))

(define (fast-Lucas n) (fast-Lucas-help n 1 1 2))

;; This function represents the table shown in the PDF.
;; Simply "hard-code" the number of recursive call you believe
;; take place for inputs 3 through 6
(define (rec-call-lucas k)
    (cond ((= k 1)  0)
          ((= k 2)  2)
          ((= k 3)  4)
          ((= k 4)  8)
          ((= k 5)  14)
          ((= k 6)  24)
    ))

;; Do the same for the fast-lucas-helper
(define (rec-call-fast-lucas-helper k)
    (cond ((= k 1)  0)
          ((= k 2)  1)
          ((= k 3)  2)
          ((= k 4)  3)
          ((= k 5)  4)
          ((= k 6)  5)
    ))

;5a Half-Companion Pells
(define (H n)
  (define (H-helper n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (* 2 (H-helper (- n 1))) (H-helper (- n 2))))))
  (if (= n 0)
      1
      (+ (H (- n 1)) (* 2 (H-helper (- n 1))))))

;5b Pells﻿
(define (P n)
  (if (= n 0)
      0
      (+ (H (- n 1)) (P (- n 1)))))

;5c (t n)
(define (t n)
  (cond ((= (modulo n 2) 0) (* 2 (expt (P n) 2)))
        ((= (modulo n 2) 1) (expt (H n) 2))))

;5d (s n)
(define (s n)
  (* (H n) (P n)))

;5e Triangular Squares
(define (tri-square n)
  (/ (* (t n) (+ (t n) 1)) 2))

;5f (square-tri n)
(define (square-tri n)
  (expt (s n) 2))

;6a Golden Ratio by continued fractions
(define (golden n)
  (if (= n 0)
      1
      (+ 1 (/ 1 (golden (- n 1))))))

;6b Golden Ratio by continued square root
(define (golden-sqrt n)
  (cond ((= n 0) 1)
        ((> n 0) (sqrt (+ 1 (golden-sqrt (- n 1)))))))

;7 explain
(define (explain-interval-sum)
  (define a "One can never do an induction on both inputs at once.")
  (define b "The base case isn't quite right. It needs to be updated to account for the two inductive calls.")
  (define c "The inductive case should be adding three things together.")
  (define d "The predicate to recognize the base case is wrong. One can go from m > n to m < n without ever seeing n = m.")
  (define e "I have no idea.")
  d)

; 8 Ackermann Function
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((and (> m 0) (= n 0)) (ack (- m 1) 1))
        ((and (> m 0) (> n 0)) (ack (- m 1) (ack m (- n 1))))))

; 9 Catalan numbers
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (catalan n)
  (if (= n 0)
      1
      (/ (factorial (* 2 n)) (* (factorial (+ n 1)) (factorial n)))))
