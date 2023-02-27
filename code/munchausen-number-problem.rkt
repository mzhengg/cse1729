(define (digits n)
  (define (helper n i)
    (if (< n 1)
        i
        (helper (/ n 10) (+ i 1))))
  (helper n 0))

(define (munchausen n)
  (define (deducter n d) (/ n (expt 10 (- d 1))))
  (define (helper n d)
    (if (= n 0)
        0
        (+ (expt (floor (deducter n (digits n))) (floor (deducter n (digits n))))
           (helper (* (- (deducter n (digits n)) (floor (deducter n (digits n)))) (expt 10 (- d 1))) d))))
  (if (= (helper n (digits n)) n)
      #t
      #f)
  (if (= (helper n (digits n)) n)
      (helper n (digits n))
      (helper n (digits n))))

(munchausen 9876)

