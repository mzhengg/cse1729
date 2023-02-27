(define (max-fg f g)
  (define (max-fg-helper x)
    (max (f x) (g x)))
  max-fg-helper)

(max-fg (lambda (x) x) (lambda (x) (expt x 2)))

(define (max-fg f g)
  (define (max-fg-helper x)
    (max (f x) (g x)))
  (max-fg-helper 10))

(max-fg (lambda (x) x) (lambda (x) (expt x 2)))

(define (max-fg f g)
  (define (max-fg-helper x)
    (max (f x) (g x)))
  max-fg-helper 10)

(max-fg (lambda (x) x) (lambda (x) (expt x 2)))