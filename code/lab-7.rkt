(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

(define (tree-size T)
  (if (null? T)
      0
      (+ 1 (tree-size (left T)) (tree-size (right T)))))

(define (tree-depth T)
  (define (helper T c)
    (if (null? T)
        (cons (- c 1) (list))
        (append (helper (left T) (+ c 1)) (helper (right T) (+ c 1)))))
  (car (helper T 0)))

(define (count-pred P tree)
  (cond ((null? tree) 0)
        ((P (value tree)) (+ 1 (count-pred P (left tree)) (count-pred P (right tree))))
        (else (+ (count-pred P (left tree)) (count-pred P (right tree))))))

(define (count-one-child T)
  (cond ((null? T) 0)
        ((and (null? (left T)) (null? (right T)))
         0)
        ((or (null? (left T)) (null? (right T)))
         (+ 1 (count-one-child (left T)) (count-one-child (right T))))
        (else (+ (count-one-child (left T)) (count-one-child (right T))))))

(define (invert-bst T)
  (if (null? T)
      '()
      (list (value T) (invert-bst (right T)) (invert-bst (left T)))))

