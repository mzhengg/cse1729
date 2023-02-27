"1a"
(define (harmonic n)
  (if (= n 1)
      1
      (+ (/ 1 n) (harmonic (- n 1)))))

"1b"
(define (Eulerest n) (abs (- (harmonic n) (log n))))

"2"
(define (count-primes m)
  (define (prime? n)
    (define (divisor? k) (= 0 (modulo n k)))
    (define (divisors-upto k)
      (and (> k 1)
           (or (divisor? k) (divisors-upto (- k 1)))))
    (not (divisors-upto (- n 1))))
  
  (define (count-primes-helper m c)
    (cond ((= m 1) c)
          ((prime? m) (count-primes-helper (- m 1) (+ c 1)))
          ((not (prime? m)) (count-primes-helper (- m 1) c))))
  (if (<= m 1)
      0
      (count-primes-helper (- m 1) 0)))

"3 Incomplete"
(define (rel-prime a b)
  (define (divides-both d)
    (and (= 0 (modulo a d))
         (= 0 (modulo b d))))
  (define (divisor-upto k)
    (and (> k 1)
         (or (divides-upto k)
             (divisor-upto (- k 1)))))
  (not (divisor-upto (min a b))))

"4a"
(define (lucas n)
  (cond ((= n 0) 2)
        ((= n 1) 1)
        ((> n 1) (+ (lucas (- n 1)) (lucas (- n 2))))))

"4b"
(define (Lucas-ratio n)
  (+ (/ (lucas n) (lucas (- n 1))) 0.0))

(define (fibonacci n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (Fibonacci-ratio n)
  (+ (/ (fibonacci n) (fibonacci (- n 1))) 0.0))

"4c"
(define (rec-call-fast-lucas-help n k lucas-a lucas-b)
  (if (= n k)
      lucas-a
      (fast-Lucas-help n (+ k 1) (+ lucas-a lucas-b) lucas-a)))

(define (rec-call-lucas n) (rec-call-fast-lucas-help n 1 1 2))

"5a"
(define (H n)
  (define (H-helper n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (* 2 (H-helper (- n 1))) (H-helper (- n 2))))))
  (if (= n 0)
      1
      (+ (H (- n 1)) (* 2 (H-helper (- n 1))))))

"5b"
(define (P n)
  (if (= n 0)
      0
      (+ (H (- n 1)) (P (- n 1)))))

"5c"
(define (t n)
  (cond ((= (modulo n 2) 0) (* 2 (expt (P n) 2)))
        ((= (modulo n 2) 1) (expt (H n) 2))))

"5d"
(define (s n)
  (* (H n) (P n)))

"5e"
(define (tri-square n)
  (/ (* (t n) (+ (t n) 1)) 2))

"5f"
(define (square-tri n)
  (expt (s n) 2))

"6a"
(define (golden n)
  (if (= n 0)
      1
      (+ 1 (/ 1 (golden (- n 1))))))

"6b"
(define (golden-sqrt n)
  (cond ((= n 0) 1)
        ((> n 0) (sqrt (+ 1 (golden-sqrt (- n 1)))))))

"7"
(define (explain-interval-sum m n)
  "This function does not work for pairs where the pair (a,b) has (b-a)=1. This is because the base case that terminates recursion is
when a and b are equivalent. However, in the then statement of the if function, m is incremented by 1 and n is decremented by 1. This
means that if, for example, m is 10 and n is 11, m will be incremented to 11 and n decremented to 10 and so on. The base cases will
be met and the recursion will occur infinitely.")

"8"
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((and (> m 0) (= n 0)) (ack (- m 1) 1))
        ((and (> m 0) (> n 0)) (ack (- m 1) (ack m (- n 1))))))

"9 Incomplete"


