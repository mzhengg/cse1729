;1a
(define (chord-length theta)
  (* 2 (sin (/ theta 2))))

;1b
(define (iradius a b c)
  (let ((s (/ (+ a b c) 2)))
    (sqrt (/ (* (- s a) (- s b) (- s c))
             s))))

;1c
(define (harmonic n)
  (define (helper i)
    (if (> i n)
        0
        (+ (/ 1 i)
           (helper (+ i 1)))))
  (helper 1))

;1b
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         1
         (or (divisor? k) (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))

(define (prime-reciprocals n)
  (define (helper i)
    (cond ((> i n) 0)
          ((prime? i) (+ (/ 1 i)
                         (helper (+ i 1))))
          (else (helper (+ i 1)))))
  (helper 2))


;2a
(define (improve g)
  (/ (+ (* g g) 1)
     (- (* 2 g) 1)))

;2b
(define (phi-approximant n)
  (define (helper a i)
    (if (> i n)
        a
        (helper (improve a)
                (+ i 1))))
  (if (>= n 1)
      (helper 1 1)
      #f))

(define (phi-approx accuracy)
  (define (p g) (- (* g g) g 1))
  (define (helper a)
    (if (<= (abs (p a)) accuracy)
        a
        (helper (improve a))))
  (helper 1))

;2d
;A scheme program is recursive when a function calls itself
;until it reaches a base case in order to compute some value.
;Tail recursion means that for every sucessive recursive call,
;one or more of the function's arguments are updated by some
;operation. This means that, unlike normal recusion, there are
;not pending operations that need to be evaluated at the end of
;the iterative process.

;3a (Confusing)
;This program doesn't work for any n that is greater than or equal to 2
;because then the base case can never be reached and the function will
;run indefinitely.
;In order to fix this, the condition (= n 0) should be rewritten as (<= n 0)
;and the condition (= n 1) should be rewritten as (<= n 1).

;3b
;1110

;3c
;240

;3d (Confusing)

;4a
(define (perfect-square? n)
  (define (helper i)
    (if (> i n)
        #f
        (if (= (sqrt n) i)
            #t
            (helper (+ i 1)))))
  (helper 1))

;4b
(define (pell-solution n x)
  (let ((f (/ (- (* x x) 1) n)))
    (if (perfect-square? f)
        #t
        #f)))

;4c
(define (pell-solve n)
  (define (helper i)
    (if (pell-solution n i)
        i
        (helper (+ i 1))))
  (helper 1))

(pell-solve 2)