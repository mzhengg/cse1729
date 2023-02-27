"1a"

(define (usd-to-euro a) (* (/ 0.84 1) a))

(usd-to-euro 250)

"1b"

(define (euro-to-yen b) (* (/ 125.92 1) b))

(euro-to-yen 250)

"1c"

(define (usd-to-yen a) (* (usd-to-euro a) (euro-to-yen 1)))

(usd-to-yen 250)

"2a"

(define e 2.71828)

"2b"

(define (tanh x) (/ (- (expt e (* 2 x)) 1) (+ (expt e (* 2 x)) 1)))

"3a"

(define (det2x2 a b c d) (- (* a d) (* b c)))

(det2x2 -3 1 2 7)

"3b"

(define (invertible? a b c d) (not (= (det2x2 a b c d) 0)))

(invertible? -3 1 2 7)

(invertible? 2 -4 -6 12)

"3ci"

(define (prod-inv-direct? a1 b1 c1 d1 a2 b2 c2 d2)

  (not (= (- (* (+ (* a1 a2) (* b1 c2)) (+ (* c1 b2) (* d1 d2))) (* (+ (* a1 b2) (* b1 d2)) (+ (* c1 a2) (* d1 c2)))) 0)))

"3cii"

(define (prod-inv-indirect? a1 b1 c1 d1 a2 b2 c2 d2)

  (not (= (* (det2x2 a1 b1 c1 d1) (det2x2 a2 b2 c2 d2)) 0)))

 

