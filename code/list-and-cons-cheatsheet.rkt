;Making a list with cons function
(define (square-list k)
  (if (= k 0)
      (list 0)
      (cons (* k k) (square-list (- k 1)))))

(square-list 4)

;Making a list with append function
(define (square-list k)
  (if (= k 0)
      (list 0)
      (append (list (* k k)) (square-list (- k 1)))))

(square-list 4)

;Appends lists together - append already exists ins scheme

(define (append-func l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append-func (cdr l1) l2))))

(append-func (list 1 2 3) (list 4 5 6))

;Applying function to each element of a list "mapping" - map function already exists in scheme
(define (mapp f items)
  (if (null? items)
      '()
      (cons (f (car items))
            (mapp f (cdr items)))))

(mapp (lambda (x) (* x x)) (list 1 2 3 4 5))

;Checks if a list is empty - null? function already exists in scheme
(define (empty? l)
  (if (null? l)
      #t
      #f))

(empty? (list))
(empty? (list 1 2 3 4))

;Reverses a list - reverse already exists in scheme
(define (reverse-func l)
  (reverse l))

(reverse-func '(1 2 3 4))
(reverse-func '(5 6 7 8))

;Messing with cons and lists
;This is a pair of two lists
(cons '(1 2 3) '(4 5 6))

;This is a list of two lists
(list '(1 2 3) '(4 5 6))

;As long as the second element is a list, when using cons, the function will append the first non-list value to the second value list
(cons 4 '( 1 2 3)) ;This happens because this is the exact form of a list ---> (value . (rest of values in list))

;When a list is the first element, cons will create a pair, where the first argument is a list and the second argument is a value.
(cons '( 1 2 3) 4)

;
(append (cons (list 1 2) '()) (cons (list 3 4) '()))
(cons (list 1 2) '())
(cons '() (list 1 2))
