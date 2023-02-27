(define (head s) (car s))
(define (rest s) (force (cdr s)))

(define (lucas-stream)
    (define (helper prev cur)
        (cons cur (delay (helper cur (+ prev cur)))))
    (helper 2 1))


(define (lucas-pseudoprime i)
  (define (helper p)
    (if (= i p)
        (lucas-stream)
        (rest (helper (+ p 1)))))
  (if (= (modulo (- (head (helper 1)) 1) i) 0)
      #t
      #f))

(define (ll-stream)
  (define (helper cur)
    (cons cur (delay (helper (- (* cur cur) 2)))))
  (helper 4))

(define (ll-test-stream c)
  (define (ll-stream-modified)
    (define (helper cur)
      (cons cur (delay (helper (if (> cur (- (expt 2 c) 1))
                                   cur
                                   (modulo (- (* cur cur) 2)
                                           (- (expt 2 c) 1)))))))
    (helper 4))
  (define (help i seq)
    (if (= i (- c 1))
        '()
        (cons (head seq)
              (help (+ i 1)
                    (rest seq)))))
  (help 0 (ll-stream-modified)))

(ll-test-stream 13)

(define (ll-test-stream c)
  (define (helper i n)
    (if (= i (- c 1))
        '()
        (cons n (helper (+ i 1)
                        (if (> n (- (expt 2 c) 1))
                            n
                            (modulo (- (* n n) 2)
                                    (- (expt 2 c) 1)))))))
  (helper 0 4))

(ll-test-stream 13)