(define (make-tree value left right)
  (list value left right))

(define (value H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))

(define heap1 (list 16 (list 21 (list 24 '() '()) (list 32 '() '())) (list 17 '() (list 58 '() '()))))
(define heap2 (list 19 (list 36 '() (list 54 '() '())) (list 26 (list 27 '() '()) (list 30 '() '()))))

(define (element? x H)
  (cond ((null? H) #f)
        ((= x (value H)) #t)
        (else (or (element? x (left H))
                  (element? x (right H))))))

(define (insert x H)
  (if (null? H)
      (make-tree x '() '())
      (let ((root-value (min x (value H)))
            (child-value (max x (value H))))
        (make-tree root-value
                   (right H)
                   (insert child-value (left H))))))

(define (largest H)
  (cond ((and (null? (left H)) (null? (right H)))
         (value H))
        ((null? (left H))
         (max (value H) (greatest (right H))))
        ((null? (right H))
         (max (value H) (greatest (left H))))
        (else
         (max (value H) (greatest (left H)) (greatest (right H))))))

(define (smallest H)
  (cond ((and (null? (left H)) (null? (right H)))
         (value H))
        ((null? (left H))
         (min (value H) (smallest (right H))))
        ((null? (right H))
         (min (value H) (smallest (left H))))
        (else
         (min (value H) (smallest (left H)) (smallest (right H))))))

(define (merge H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
    ((= (value H1) (value H2))
         (make-tree (value H1)
                    H2
                    (merge (left H1) (right H2))))
        ((< (value h1) (value H2))
         (make-tree (value H1)
                    H2
                    (merge (left H1) (right H1))))
        ((> (value H1) (value H2))
         (make-tree (value H2)
                    H1
                    (merge (left H2) (right H2))))))

(define (remove x H)
  (if (element? x H)
      (cond ((= x (value H))
             (merge (left H) (right H)))
            ((element? x (left H))
             (make-tree (value H)
                        (remove x (left H))
                        (right H)))
            ((element? x (right H))
             (make-tree (value H)
                        (left H)
                        (remove x (right H)))))
      #f))
         
        
        