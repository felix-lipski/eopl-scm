(define list-length
  (lambda (xs)
    (if (null? xs)
        0
        (+ 1 (list-length (cdr xs))))))


(define nth-element
  (lambda (xs n)
    (if (null? xs)
        "index error"
        (if (zero? n)
            (car xs)
            (nth-element (cdr xs) (- n 1))))))


(define remove-first
  (lambda (sym xs)
    (if (null? xs)
        '()
        (if [eq? (car xs) sym]
            [cdr xs]
            [cons (car xs) (remove-first sym (cdr xs))]))))

(remove-first 'x '(t s x g y))


(define occurs-free
  (lambda (sym exp)
    (cond
     [(symbol? exp) (eq? sym exp)]
     [(eq? (car exp) 'lambda)
      (if (eq? (car (car (cdr exp))) sym)
          #f
          (occurs-free sym (cdr (cdr exp))))]
     [else
      (or
       [occurs-free sym (car exp)]
       [occurs-free sym (cadr exp)])])))

(occurs-free 'x 'x)
(occurs-free 'x 'y)
(occurs-free 'x '(lambda (x) (x y)))
(occurs-free 'x '(lambda (y) (x y)))
(occurs-free 'x '((lambda (x) x) (x y)))
(occurs-free 'x '(lambda (y) (lambda (z) (x (y z)))))


(define subst
  (lambda (new old sexp)
    (cond
     [(symbol? sexp)
      (if (eq? sexp old)
          new
          sexp)]
     [(null? sexp) '()]
     [else
      (cons
       [subst new old (car sexp)]
       [subst new old (cdr sexp)])])))

(subst 'a 'b '((b c) (b () b d)))


(define subst-map
  (lambda (new old sexp)
    (cond
     [(symbol? sexp)
      (if (eq? sexp old)
          new
          sexp)]
     [(null? sexp) '()]
     [else (map (lambda (x) (subst-map new old x)) sexp)])))

(subst-map 'a 'b '((b c) (b () b d)))


(define number-elements-from
  (lambda (xs n)
    (if (null? xs)
        '()
        [cons
         (list n (car xs))
         (number-elements-from (cdr xs) (+ n 1))])))

(define number-elements
  (lambda (xs)
    (number-elements-from xs 1)))

(number-elements '(a b c))


(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        [cons x (duple (- n 1) x)])))

(duple 3 5)


(define invert
  (lambda (xs)
    (map (lambda (pair) [list (cadr pair) (car pair)]) xs)))

(invert (number-elements (duple 4 'a)))


(define down
  (lambda (xs)
    (map (lambda (x) [list x]) xs)))

(down (duple 4 'a))


(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (x)
           (if (symbol? x)
               (cond
                [(eq? x s1) s2]
                [(eq? x s2) s1]
                [else x])
               (swapper s1 s2 x)))
     slist)))

(swapper 'x 'y '((x) y (z (x))))


(define list-set
  (lambda (xs n x)
    (if (zero? n)
        (cons x (cdr xs))
        (cons (car xs) (list-set (cdr xs) (- n 1) x)))))

(list-set '(a b c d) 2 '(1 2))


(define count-occurances
  (lambda (el sexp)
    (if (symbol? sexp)
        (if (eq? el sexp) 1 0)
        (fold-left + 0 (map
          (lambda (x)
            (count-occurances el x))
          sexp)))))

(count-occurances 'x '((f x) y (((x z) () x))))


(define product
  (lambda (sos1 sos2)
    (apply append (map
     (lambda (x) (map (lambda (y) (list x y)) sos2))
     sos1))))

(product '(a b c) '(x y))


(define filter-in
  (lambda (pred xs)
    (cond
     [(null? xs) '()]
     [(pred (car xs))
      (cons (car xs) (filter-in pred (cdr xs)))]
     [else (filter-in pred (cdr xs))])))

(filter-in number? '(a 2 '(1 3) 5))


(define list-index-acc
  (lambda (pred xs n)
    (cond
     [(null? xs) #f]
     [(pred (car xs)) n]
     [else (list-index-acc pred (cdr xs) (+ n 1))])))

(define list-index
  (lambda (pred xs)
    (list-index-acc pred xs 0)))

(list-index number? '(a b 73 d e))


(define every?
  (lambda (pred xs)
    (if (null? xs)
        #t
        (and
         [pred (car xs)]
         [every? pred (cdr xs)]))))

(every? number? '(1 2 3 4))
(every? number? '(1 2 a 4))


(define exists?
  (lambda (pred xs)
    (cond
     [(null? xs) #f]
     [(pred (car xs)) #t]
     [else (exists? pred (cdr xs))])))

(exists? symbol? '(1 2 3 4))
(exists? symbol? '(1 2 a 4))


(define up
  (lambda (xs)
    (apply append xs)))

(up '((1 2) (3 4)))


(define flatten
  (lambda (sexp)
    (if (list? sexp)
        (if (null? sexp)
            '()
            (up (map flatten sexp)))
        (list sexp))))

(flatten '(a (b (c 1)) (d 2)))


(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (append
         [sort (filter-in (lambda (x) (<= x (car loi))) (cdr loi))]
         [list (car loi)]
         [sort (filter-in (lambda (x) (> x (car loi))) (cdr loi))]))))

(sort '(1 2 5 4 3))
(sort '(5 4 3 2 1))


(define merge
  (lambda (loi1 loi2)
    (sort (append loi1 loi2))))

(merge '(20 0 5 11) '(1 7 4 22))


(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (append
         [sort/predicate pred (filter-in (lambda (x) (pred x (car loi))) (cdr loi))]
         [list (car loi)]
         [sort/predicate pred (filter-in (lambda (x) (not (pred x (car loi)))) (cdr loi))]))))

(sort/predicate < '(8 4 6 2 1 5))
(sort/predicate > '(8 4 6 2 1 5))
