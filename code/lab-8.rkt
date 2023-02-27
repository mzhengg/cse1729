(define (make-heap val left right) (list val left right))

(define (value H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))


(define (heap-insert f x H)
  (cond ((null? H) (make-heap x '() '()))
        ((f x (value H)) (make-heap x (right H) (heap-insert f (value H) (left H))))
        ((f (value H) x) (make-heap (value H) (right H) (heap-insert f x (left H))))))

(define (combine f Ha Hb)
  (cond ((null? Ha) Hb)
        ((null? Hb) Ha)
        ((f (value Ha) (value Hb))
         (make-heap (value Ha) Hb (combine f (left Ha) (right Ha))))
        ((f (value Hb) (value Ha))
         (make-heap (value Hb) Ha (combine f (left Hb) (right Hb))))))

(define (empty? H)
  (if (null? H)
      #t
      #f))

(define (heap-remove f H) (combine f (left H) (right H)))
                
