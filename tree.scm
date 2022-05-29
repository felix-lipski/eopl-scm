;; bintree ::= Int | (Symbol Bintree Bintree)

(define leaf
  (lambda (x) x))

(leaf 4)


(define branch
  (lambda (label ch1 ch2)
    (list label ch1 ch2)))

(branch 'red (leaf 3) (branch 'blue (leaf 1) (leaf 8)))


(define leaf? number?)

(leaf? (leaf 4))


(define lson
  (lambda (tree)
    (if (leaf? tree)
        (error tree "tried applying lson to a leaf")
        (cadr tree))))

(lson (branch 'red (leaf 3) (branch 'blue (leaf 1) (leaf 8))))


(define rson
  (lambda (tree)
    (if (leaf? tree)
        (error tree "tried applying rson to a leaf")
        (cadr (cdr tree)))))

(rson (branch 'red (leaf 3) (branch 'blue (leaf 1) (leaf 8))))


(define contents-of
  (lambda (tree)
    (if (leaf? tree)
        tree
        (car tree))))

(contents-of (branch 'green (leaf 1) (leaf 5)))
(contents-of (leaf 1))


(define double-tree
  (lambda (tree)
    (if (leaf? tree)
        (leaf (* 2 tree))
        (branch
         [contents-of tree]
         [double-tree (lson tree)]
         [double-tree (rson tree)]))))

(double-tree (branch 'red (leaf 3) (branch 'blue (leaf 1) (leaf 8))))


(define mark-leaves-with-red-depth-acc
  (lambda (tree n)
    (if (leaf? tree)
        (leaf n)
        (let ((n2 (if (eqv? (contents-of tree) 'red) (+ n 1) n)))
          (branch
           [contents-of tree]
           [mark-leaves-with-red-depth-acc (lson tree) n2]
           [mark-leaves-with-red-depth-acc (rson tree) n2])))))

(define mark-leaves-with-red-depth
  (lambda (tree) (mark-leaves-with-red-depth-acc tree 0)))

(mark-leaves-with-red-depth
 (branch 'red
  (leaf 3)
  (branch 'blue
   (branch 'red (leaf 1) (leaf 6))
   (leaf 8))))


(define path
  (lambda (n tree)
    (if (leaf? tree)
        (if (eq? tree n)
            '()
            #f)
        (let [(found-in-left (path n (lson tree)))
              (found-in-right (path n (rson tree)))]
          (cond
           [found-in-left (cons 'left found-in-left)]
           [found-in-right (cons 'right found-in-right)]
           [else #f]))
          )))

(path 3
 (branch 'red
  (branch 'red (leaf 8) (leaf 3))
  (leaf 8)))

(path 6
 (branch 'red
  (leaf 3)
  (branch 'blue
   (branch 'red (leaf 1) (leaf 6))
   (leaf 8))))


(define number-leaves-acc
  (lambda (tree max)
    (if (leaf? tree)
        (list (leaf max) (+ 1 max))
        (let* ([left (number-leaves-acc (lson tree) max)]
               [right (number-leaves-acc (rson tree) (cadr left))])
          (list (branch (contents-of tree) (car left) (car right)) (cadr right))))))

(define number-leaves
  (lambda (tree) (car (number-leaves-acc tree 0))))

(number-leaves
 (branch 'foo
  (branch 'bar
   (leaf 26)
   (leaf 12))
  (branch 'baz
   (leaf 11)
   (branch 'quux
    (leaf 117)
    (leaf 14)))))


(define number-elements
  (lambda (xs)
    (if (null? xs) '()
        (g (list 0 (car xs)) (number-elements (cdr xs))))))

(define g
  (lambda (x xs)
    (cons x (map
             (lambda (p)
               (cons (+ 1 (car p)) (cdr p)))
             xs))))

(number-elements '(a b c))
