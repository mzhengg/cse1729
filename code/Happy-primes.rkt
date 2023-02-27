;1a
(define (num->list n)
  (let* ((a (/ n 10))
         (b (floor a))
         (c (* 10 (- a b))))
    (if (< n 10)
        (list n)
        (append (list c) (num->list b)))))

;1b
(define (square l)
  (if (null? l)
      0
      (+ (expt (car l) 2)
         (square (cdr l)))))

;1c
(define (make-tree value left right)
  (list value left right))

(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

(define (insert n T)
  (cond ((null? T) (make-tree n '() '()))
        ((= n (value T)) T)
        ((< n (value T)) (make-tree (value T)
                                    (insert n (left T))
                                    (right T)))
        ((> n (value T)) (make-tree (value T)
                                    (left T)
                                    (insert n (right T))))))

(define (element? n T)
  (cond ((null? T) #f)
        ((= n (value T)) #t)
        ((> n (value T)) (element? n (right T)))
        ((< n (value T)) (element? n (left T)))))

;1d
(define (is-happy? n)
  (let ((T '()))
    (


                                    
  