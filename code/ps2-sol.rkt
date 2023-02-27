"1A"
(define (fizzbuzz x)
  (cond ((and (= (modulo x 3) 0) (= (modulo x 5) 0)) "fizzbuzz")
        ((= (modulo x 3) 0) "fizz")
        ((= (modulo x 5) 0) "buzz")
        (else x)
  )
)

(fizzbuzz 31)

"1B"
(define (fizz x) "fizz")

(define (buzz x) "buzz")

(define (fizzbuzz2 x)
  (cond ((and (= (modulo x 3) 0) (= (modulo x 5) 0)) (string-append (fizz x) (buzz x)))
        ((= (modulo x 3) 0) (fizz x))
        ((= (modulo x 5) 0) (buzz x))
        (else x)

  )
)

(fizzbuzz 9)

"2"
(define (piecewise x)
  (cond ((> x (* 2 3.142)) (- x (* 2 3.142)))
        ((and (>= x (* -1 3.142)) (<= x (* 2 3.142))) (sin x))
        ((< x (* -1 3.142)) (- (* -1 x) 3.142))
  )
)

(piecewise 0)

"3"
(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (add n m)
  (cond ((= n 0) m)
        ((> n 0) (inc (add (- n 1) m)))
  )
)

(add 9 10)

"4"
(define (mult n m)
  (cond ((= n 0) 0)
        ((> n 0) (add m (mult (dec n) m)))
  )
)

(mult 3 10)

"5"
(define (power b n)
  (cond ((= n 0) 1)
        ((> n 0) (mult b (power b (dec n))))
        )
  )

(power 2 1)

"6"
(define (raise x n)
  (cond ((= n 0)
         1)
        ((= (modulo n 2) 0)
         (mult (power x (/ n 2))
               (power x (/ n 2))))
        ((= (modulo n 2) 1)
         (mult (mult (power x (floor (/ n 2))) (power x (floor (/ n 2)))) x))
        )
  )

(raise 3 1)

"7"
(define (sumEven n)
  (cond ((= n 0) 0)
        ((= (modulo n 2) 0) (+ n (sumEven (- n 2))))
        ((= (modulo n 2) 1) (+ (- n 1) (sumEven (- n 3))))
      )
  )

(sumEven 11)

(define (sumOdd n)
  (cond ((< n 0) 0)
        ((= (modulo n 2) 1) (+ n (sumOdd (- n 2))))
        ((= (modulo n 2) 0) (+ (- n 1) (sumOdd (- n 3))))
      )
  )

(sumOdd 10)

"8"
(define (helper-h x)
  (- 1 (/ 1 x))
  )

(define (h-product k)
  (cond ((= k 1) 1)
        ((= k 2) 0.5)
        ((> k 2) (* (helper-h k) (h-product (- k 1))))
      )
  )

(h-product 1)

"9"
(define (divides a b)
  (= 0 (modulo b a))
  )

(divides 2 4)

(define (divisors-upto n k)
  (cond ((= k 0) 0)
        ((= n 0) 0)
        ((= k 1) 1)
        ((divides k n) (+ 1 (divisors-upto n (- k 1))))
        ((not (divides k n)) (divisors-upto n (- k 1)))
        )
  )

(define (divisors n) (divisors-upto n n))

(divisors 10)

"10"
(define (subfact n)
  (cond ((= n 0) 1)
        ((= n 1) 0)
        ((> n 1) (* (- n 1) (+ (subfact (- n 1)) (subfact (- n 2)))))
        )
  )
(subfact 4)

"11"
(define (factorial n)
  (cond ((= n 0) 1)
        ((> n 0) (* n (factorial (- n 1))))
        )
  )

(define (new-cos x n)
  (cond ((= n 0) 1)
        ((> n 0) (+ (* (expt -1 n) (/ (expt x (* 2 n)) (factorial (* 2 n)))) (new-cos x (- n 1))))
        )
  )

(new-cos 1 3)