; Int x Val -> List-of-Val
(define duple
  (lambda (n x)
    (duple-list-element n '() x)))

; Int x List x Val -> List
(define duple-list-element
  (lambda (n lst x)
    (if (zero? n)
        lst
        (duple-list-element (- n 1) (cons x lst) x))))

; List -> List
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (invert-pair (car lst)) (invert (cdr lst))))))

; Pair -> Pair
(define invert-pair
  (lambda (pair)
    (list (cadr pair) (car pair))))

; List -> List
(define down
  (lambda (lst)
    (if (null? lst)
        lst
        (cons (list (car lst)) (down (cdr lst))))))

; Sym x Sym x List -> List
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (swap-symbols s1 s2 slist)
            (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))))))

; Sym x Sym x List -> List
(define swap-symbols
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (eqv? s1 (car slist))
            (cons s2 (swapper s1 s2 (cdr slist)))
            (if (eqv? s2 (car slist))
                (cons s1 (swapper s1 s2 (cdr slist)))
                (cons (car slist) (swapper s1 s2 (cdr slist))))))))

; List x Int x Val -> List
(define list-set
  (lambda (lst n x)
    (substitute-element lst n x 0)))

; List x Int x Int -> List
(define substitute-element
  (lambda (lst n x i)
    (if (eq? i n)
        (cons x (cdr lst))
        (cons (car lst) (substitute-element (cdr lst) n x (+ i 1))))))

; Symbol x SymbolList -> Int
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (if (symbol? (car slist))
            (if (eqv? s (car slist))
                (+ 1 (count-occurrences s (cdr slist)))
                (count-occurrences s (cdr slist)))
            (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist)))))))
            
; List x List -> List
(define product
  (lambda (sos1 sos2)
    (if (< (length sos1) (length sos2))
        (product sos2 sos1)
        (cartesian-product sos1 sos2 sos2))))

; List x List x List -> List
(define cartesian-product
  (lambda (sos1 sos2 sos22)
    (if (null? sos1)
        '()
        (if (null? sos2)
            (cartesian-product (cdr sos1) sos22 sos22)
            (cons (list (car sos1) (car sos2)) (cartesian-product sos1 (cdr sos2) sos22))))))

; Predicate x List -> List
(define (filter-in pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))

; Predicate x List -> Int / #f
(define (list-index pred lst)
  (list-index-count pred lst 0))

; Predicate x List x Int -> Int / #f
(define (list-index-count pred lst c)
  (if (null? lst)
      #f
      (if (pred (car lst))
          c
          (list-index-count pred (cdr lst) (+ 1 c)))))

; Predicate x List -> #t / #f
(define (every? pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (every? pred (cdr lst))
          #f)))

; Predicate x List -> #t / #f
(define (exists? pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (exists? pred (cdr lst)))))

; List -> List
(define (up lst)
  (if (null? lst)
      '()
      (up-it (car lst) (cdr lst))))
      
; Val x List -> List
(define (up-it current remaining)
  (if (and (null? remaining) (null? current))
      '()
      (cond ((null? remaining) 
             (if (list? current)
                 (cons (car current) (up-it (cdr current) '()))
                 (cons current '())))
            ((null? current) (up-it (car remaining) (cdr remaining)))
            ((list? current) (cons (car current) (up-it (cdr current) remaining)))
            (else (cons current (up-it (car remaining) (cdr remaining)))))))
  
; List -> List
(define (flatten lst)
  (if (null? lst)
      '()
      (if (list? (car lst))
          (flatten (flatten-concat (car lst) (cdr lst)))
          (cons (car lst) (flatten (cdr lst))))))

; List x List -> List
(define (flatten-concat lst src)
  (if (null? lst)
      src
      (cons (car lst) (flatten-concat (cdr lst) src))))

; ListOfInt x ListOfInt -> ListOfInt
(define (merge loi1 loi2)
  (if (and (null? loi1) (null? loi2))
      '()
      (cond ((null? loi1) loi2)
            ((null? loi2) loi1)
            ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
            (else (cons (car loi2) (merge loi1 (cdr loi2)))))))

; ListOfInt -> ListOfInt
(define (sort loi)
  (let* ((erg (sort-it < loi))
        (erg2 (sort-it < erg)))
        (if (equal? erg erg2)
            erg
            (sort erg2))))

; ListOfInt -> ListOfInt
(define (sort-it pred loi)
  (if (null? loi)
      '()
      (if (= (length loi) 1)
          (cons (car loi) '())
          (if (pred (car loi) (cadr loi))
              (cons (car loi) (sort-it pred (cdr loi)))
              (cons (cadr loi) (sort-it pred (cons (car loi) (cddr loi))))))))
      

; Predicate x ListOfInt -> ListOfInt
(define (sort/predicate pred loi)
  (let* ((erg (sort-it pred loi))
        (erg2 (sort-it pred erg)))
        (if (equal? erg erg2)
            erg
            (sort/predicate pred erg2))))

; Bintree ::= Int | (Symbol Bintree Bintree)

; Int -> Inr
(define (leaf int)
  int)

; Symbol x Bintree x Bintree -> Bintree
(define (interior-node sym branch1 branch2)
  (cons sym (list branch1 branch2)))

; Bintree -> Boolean
(define (leaf? elem)
  (if (list? elem)
      #f
      #t))

; Bintree -> Bintree
(define (lson node)
  (cadr node))

; Bintree -> Bintree
(define (rson node)
  (caddr node))

; Bintree -> Symbol/Int
(define (contents-of elem)
  (if (leaf? elem)
      elem
      (car elem)))

; Bintree -> Bintree
(define (double-tree tree)
  (if (leaf? tree)
      (* 2 (contents-of tree))
      (cons (contents-of tree) (list (double-tree (lson tree)) (double-tree (rson tree))))))

; Bintree -> Bintree
(define (mark-leaves-with-red-depth tree)
  (red-depth tree 0))

; Bintree x Int -> Bintree
(define (red-depth tree depth)
  (if (leaf? tree)
      depth
      (if (eq? (contents-of tree) 'red)
          (cons (contents-of tree) (list (red-depth (lson tree) (+ 1 depth)) (red-depth (rson tree) (+ 1 depth))))
          (cons (contents-of tree) (list (red-depth (lson tree) depth) (red-depth (rson tree) depth))))))

; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)

; Int x Binary-search-tree -> List / #f
; Searches for n in binary-search-tree bst and returns a list of lefts and rights showing how to find the node containing n. #f is returned if n is not found.
(define (path n bst)
  (if (null? bst)
      #f
      (if (= (car bst) n)
          '()
          (let ((left (path n (cadr bst)))
                (right (path n (caddr bst))))
            (if (eq? left #f)
              (if (eq? right #f)
                  #f
                  (cons 'right right))
              (cons 'left left))))))

; Bintree -> Bintree
; Numbers the leaves of the tree starting from 0
(define (number-leaves tree)
  (number-leaves-count tree (number-leaves-counter -1)))

; Int -> Function
; Returns a counter function initialized with start. Each invocation increments this counter by one.
(define (number-leaves-counter start)
  (lambda () 
    (set! start (+ start 1))
    start))

; Bintree x Function -> Bintree
; Recreates the bintree with all leaves numbered using the counter function's values
(define (number-leaves-count tree count)
  (if (leaf? tree)
      (count)
      (cons (contents-of tree) (list (number-leaves-count (lson tree) count) (number-leaves-count (rson tree) count)))))

; List -> Listof(List(Int SchemeVal))
; (100 200 300) -> ((0 100) (1 200) (2 300))
(define (number-elements lst)
  (if (null? lst)
      '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))

; List x List -> Listof(List(Int SchemeVal))
(define (g pair lst)
  (g-unit (cons pair lst) (number-leaves-counter (- (car pair) 1))))

; Listof(List(Int SchemeVal)) x Function -> Listof(List(Int SchemeVal))
; Rewrites the given list of pairs such that every pair has the value of the counter function as value.
; E.g. ((0 100) (0 200) (0 300)) -> ((0 100) (1 200) (2 300)) if an incremental counter function is used.
(define (g-unit lst counter)
  (if (null? lst)
      '()
      (cons (cons (counter) (cdar lst)) (g-unit (cdr lst) counter))))

