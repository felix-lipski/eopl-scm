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
