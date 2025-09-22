#lang racket
(provide (all-defined-out))

;; A pairing heap is an N-ary tree that respects the heap
;; property and efficiently implements the following operations:
;; - insertion into the heap
;; - deletion of the root (restructuring the heap)
;; - merging of two heaps
;; The heap property refers to maintaining an ordering
;; relationship between any parent node and its children:
;; - in a min-heap, the value of the parent is less than or
;;   equal to the values ​​of its children
;; - in a max-heap, the value of the parent is greater than or
;;   equal to the values ​​of its children
;; - a heap can also rely on other ordering relationships
;;
;; We will represent a pairing heap (abbreviated PH - from
;; "pairing heap") as a list:
;; - empty, if the heap contains no elements
;; - (root child_1 child_2 ... child_n), otherwise,
;;   where each child is also a PH
;;
;; In this phase we implement a pairing max-heap.


; TODO 1
; Define, as indicated, the following constructors and
; operators of the PH type.
; Then, manipulate the PH through this interface
; (do not use functions dedicated to lists when there are
; equivalent functions dedicated to the PH type).

; empty-ph : PH
; out: empty PH
(define empty-ph null)

; val->ph : T -> PH
; in: a value of some type T
; out: the PH containing only this value
(define (val->ph t)
  (list t))

; ph-empty? : PH -> Bool
; in: pairing heap ph
; out: true, if ph is empty
;      false, otherwise
(define (ph-empty? ph)
  (if (null? ph)
      #t
      #f))

; ph-root : PH -> T | Bool
; in: pairing heap ph
; out: false, if ph is empty
;      root(ph), otherwise
(define (ph-root ph)
  (if (ph-empty? ph)
      #f
      (car ph)))

; ph-subtrees : PH -> [PH] | Bool
; in: pairing heap ph
; out: false, if ph is empty
;      children(ph), otherwise
(define (ph-subtrees ph)
  (if (ph-empty? ph)
      #f
      (cdr ph)))


; TODO 2
; merge: PH x PH -> PH
; in: pairing heaps ph1, ph2
; out: union(ph1, ph2) as such:
;  - union(empty, soemthing) = something
;  - otherwise, the PH with the smaller root becomes
;    the first child of the one with the larger root
;    (by convention, if the roots are
;    equal, ph2 becomes the child of ph1)
; ATTENTION!
; merge is not commutative
(define (merge ph1 ph2)
  (cond
    ((ph-empty? ph1) ph2)
    ((ph-empty? ph2) ph1)
    ((> (ph-root ph2) (ph-root ph1)) (append (val->ph (ph-root ph2)) (list ph1) (cdr ph2)))
    (else (append (val->ph (ph-root ph1)) (list ph2) (cdr ph1)))))


; TODO 3
; ph-insert : T x PH -> PH
; in: value val, pairing heap ph
; out: ph' result after inserting val into ph
;  - insertion is a merge between ph and
;    PH created only from val value
;    (in this order)
(define (ph-insert val ph)
  (merge ph (val->ph val)))


; TODO 4
; list->ph : [T] -> PH
; in: list of values lst
; out: ph' result of repeated insertions
;  - insert the last element of lst into the empty PH
;  - ...
;  - insert the first element of lst into the current PH
; RESTRICTIONS:
;  - Use stack recursion.
(define (list->ph lst)
  (if (null? lst)
      empty-ph
      (ph-insert (car lst) (list->ph (cdr lst)))))

; TODO 5
; two-pass-merge-LR : [PH] -> PH
; in: list of PHs phs
; out: ph' result from merge left-right:
;  - merge first two PHs
;  - merge result with merge of next two
;  ...
;  - merge result with:
;    - merge last two PHs, if nr_even(phs)
;    - last PH, if nr_odd(phs)
; RESTRICTIONS:
;  - Use tail recursion.
(define (two-pass-merge-LR-helper phs acc)
  (cond
    ((ph-empty? phs) acc)
    ((= (length phs) 1) (merge acc (car phs)))
    (else (two-pass-merge-LR-helper (cddr phs) (merge acc (merge (car phs) (cadr phs)))))))

(define (two-pass-merge-LR phs)
  (two-pass-merge-LR-helper phs null))


; TODO 6
; two-pass-merge-RL : [PH] -> PH
; in: list of PHs phs
; out: ph' result from merge right-left
; (as above, but start with the last two PHs:
;  - merge the penultimate with the last
;  - merge result with merge from the previous two, etc.)
; RESTRICTIONS:
;  - Use stack recursion.
(define (two-pass-merge-RL phs)
  (cond
    ((ph-empty? phs) null)
    ((= (modulo (length phs) 2) 1) (merge (two-pass-merge-RL (cdr phs)) (car phs)))
    (else (merge (two-pass-merge-RL (cddr phs)) (merge (car phs) (cadr phs))))))


; TODO 7
; tournament-merge : [PH] -> PH
; in: list of PHs phs
; out: ph' result from "knock-out" merge
;  - ph lists are traversed left-right
;  - merge two by two (for odd number, last one remains as is)
;  - merge two by two between previously resulting PHs
;  ...
;  - until only one PH remains
(define (tournament-merge-helper phs acc)
  (cond
    ((ph-empty? phs) acc)
    ((= (length phs) 1) (append acc (list (car phs))))
    (else (tournament-merge-helper (cddr phs) (append acc (list (merge (car phs) (cadr phs))))))))

(define (tournament-merge phs)
  (cond
    ((ph-empty? phs) phs)
    ((<= (length phs) 1) (car phs))
    (else (tournament-merge (tournament-merge-helper phs null)))))
        

; TODO 8
; ph-del-root : PH -> PH | Bool
; in: pairing heap ph
; out: false, if ph is empty
;      ph' resulting from deleting root(ph), otherwise
;      - the children of root(ph) are merged by two-pass-merge-LR
(define (ph-del-root ph)
  (if (ph-empty? ph)
      #f
      (two-pass-merge-LR (ph-subtrees ph))))
