(define (make-bintree datum left right)
  (lambda (msg)
    (cond ((= msg 0) datum)
          ((= msg 1) left)
          ((= msg 2) right))))

(define (datum bintree) (bintree 0))
(define (left bintree) (bintree 1))
(define (right bintree) (bintree 2))

(define tet 'toasterhead)  ; tet = the empty tree
(define (empty-tree? tree) 
  (eq? tree tet))
(define (leaf? bintree)
  (and (not (empty-tree? bintree))
       (empty-tree? (left bintree))
       (empty-tree? (right bintree))))

(define (two-branch? tree)
  (and (not (empty-tree? tree))
       (not (empty-tree? (left tree)))
       (not (empty-tree? (right tree)))))

(define (right-empty? tree)
  (and (not (leaf? tree))
       (empty-tree? (right tree))))

(define (left-empty? tree)
  (and (not (leaf? tree))
       (empty-tree? (left tree))))


(define bst
  (make-bintree 15
     (make-bintree 6
        (make-bintree 2 tet tet)
        tet)
     (make-bintree 22
        (make-bintree 17
           (make-bintree 16 tet tet)
           (make-bintree 19 tet tet))
        (make-bintree 24 tet tet))))

;1
(define (contains? tree n)
  (or (= (datum tree) n)
      (if (and (< n (datum tree)) (not (empty-tree? (left tree))))
          (contains? (left tree) n) #f)
      (if (and (> n (datum tree)) (not (empty-tree? (right tree))))
          (contains? (right tree) n) #f)))


;2
(define (inorder tree)
  (cond ((empty-tree? tree) nil)
        (else (append (inorder (left tree)) (list (datum tree)) (inorder (right tree))))))

;3
(define (count-nodes tree)
  (length (inorder tree)))

;4
(define (square x)
  (* x x))
                

(define (square-tree tree)
  (if (empty-tree? tree)
      0
      (make-bintree (square (datum tree)) (square-tree (left tree)) (square-tree (right tree)))))
                
;5
(define (map-tree proc tree)
  (if (empty-tree? tree)
      0
      (make-bintree (proc (datum tree)) (map-tree proc (left tree)) (map-tree proc (right tree)))))
        
;1
(define nil '())

(define (first-n lyst n)
  (define (helper new old m)
    (if (= m n)
        new
        (helper (append new (list (car old))) (cdr old) (+ 1 m))))
  (helper nil lyst 0))

;2
(define (part1 lyst)
  (cond ((null? lyst) nil)
        ((even? (length lyst)) (first-n lyst (/ (length lyst) 2)))
        (else (first-n lyst (floor (/ (length lyst) 2))))))

;3
(define (last-n lyst n)
  (define (helper newlyst m)
    (if (= m n)
        newlyst
        (helper (cdr newlyst) (+ 1 m))))
  (helper lyst 0))

(define (rest lyst)
  (cond ((null? lyst) nil)
        ((odd? (length lyst)) (last-n lyst (floor (/ (length lyst) 2))))
        (else (last-n lyst (/ (length lyst) 2)))))

;4
(define (middle-datum lyst)
  (car (rest lyst)))

;5
(define (part2 lyst)
  (cdr (last-n lyst (floor (/ (length lyst) 2)))))
