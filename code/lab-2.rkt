"1"
(define (geom-series-np2 n)

  (if (= n 0)

      1

      (+ (/ 1 (expt 2 n)) (geom-series-np2 (- n 1)))

  )
)

(geom-series-np2 2)

"2"
(define (num-digits n)

  (cond ((= n 0) 1)

        ((and (> n 0) (< n 1)) 0)

        ((= n 1) 1)

        ((> n 1) (+ 1 (num-digits (/ n 10))))
  )
)

(num-digits 10000)

"3a"
(define (a n)
  
  (if (= n 1)
      
      2
      
      (* 2 (a (- n 1)))
  )
)

(a 2)

"3b"
(define (num-ancestors n)

  (if (= n 1)

      2

      (+ (a n) (num-ancestors (- n 1)))

  )
)

(num-ancestors 2)

"4"
(define (factorial n)

  (if (= n 0)

      1

      (* n (factorial (- n 1)))

  )
)

(factorial 4)

(define (n-choose-k n k)

  (/ (factorial n) (* (factorial (- n k)) (factorial k)))

)

(n-choose-k 5 2)

"5"

(define (pascals-triangle n k)

  (cond ((< k 0) 0)

        ((< n k) 0)

        ((and (= k 0) (= n 0)) 1)

        ((and (= n 1) (<= k n)) 1)

        (else (+ (n-choose-k (- n 1) k) (n-choose-k (- n 1) (- k 1))))
  )
)

(pascals-triangle 10 4)