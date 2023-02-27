(define (value s) (car s))
(define (rest s) (force (cdr s)))

(define (integer-stream n)
  (cons n (delay (integer-stream (+ n 1)))))

(define (divides? x y)
  (if (= (modulo x y) 0)
      #t
      #f))

(define (sift k numbers)
  (cond ((divides? (value numbers) k)
         (sift k (rest numbers)))
        (else
         (cons (value numbers)
               (delay (sift k (rest numbers)))))))

(define (prime-stream stream)
  (cons (value stream)
        (delay (prime-stream (sift (value stream)
                                   (rest stream))))))

(define primes (prime-stream (integer-stream 2)))

(rest (rest primes))



