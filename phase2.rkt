#lang racket
(require racket/match)
(provide (all-defined-out))

;; In this phase we abstract the PH type operators in such a way
;; that we can easily derive the operations for different variants
;; of PH, depending on the ordering relationship on which
;; the heap property is based.
;;  - directly affected function: merge
;;  - indirectly affected functions: functions that call merge,
;;    which will need to receive the merge type as a parameter
;;
;; Then, we use the PH type to process movies, where a movie
;; is represented as a structure with 5 fields: name, rating, genre,
;; duration, others.
;;
;; The recommended workflow for step 2 is:
;;  - Copy from step 1 the functions that remain unchanged
;;  - Abstract by the ordering relationship:
;;   * define the more general operator merge-f which receives, in addition
;;     to merge, a comparator by which the elements should be ordered
;;   * derive from this operator the required variants of merge
;;   * modify those functions in stage 1 that call merge, so
;;     that the merge function is a parameter of the function, not an
;;     identifier bound to an external value
;;  - Read the tutorial on structures in Racket (file "tutorial.rkt")
;;  - Implement the functions that process movies


; TODO 0
; Copy the functions from phase1

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

; TODO 1
; Define the function merge-f in curry form,
; so that later you define the point-free
; functions merge-min, merge-max and
; merge-max-rating, as partial applications
; of merge-f.
;  - point-free definition = a definition that
;    does not make the argument of the function explicit
;   * ex: (define f add1) is a point-free definition
;   * ex: (define (f x) (add1 x)) or, equivalently,
;     (define f (λ (x) (add1 x))) are not point-free
; merge-f = merge with comparison criterion comp
; in: pairing heaps ph1, ph2, comparator comp
;     (order and grouping of parameters
;     to be decided by you)
; out: union(ph1, ph2) like this:
;  - union(void, any) = any
;  - otherwise, the PH with root "less comp"
;    becomes the first child of the other
;    (in case of a tie, ph2 becomes the son of ph1)
(define (merge-f comp-func)
  (lambda (ph1 ph2)
    (cond
      ((ph-empty? ph1) ph2)
      ((ph-empty? ph2) ph1)
      ((> (comp-func ph1 ph2) 0) (append (val->ph (ph-root ph2)) (list ph1) (cdr ph2)))
      (else (append (val->ph (ph-root ph1)) (list ph2) (cdr ph1))))))

; merge-max : PH x PH -> PH
; in: pairing heaps ph1, ph2
; preconditions: ph1, ph2 are max-PHs
; out: max-PH resulting from union(ph1, ph2)
; RESTRICTIONS:
;  - The definition must be point-free.
(define (comp-max ph1 ph2)
  (- (ph-root ph2) (ph-root ph1)))

(define merge-max (merge-f comp-max))

; merge-min : PH x PH -> PH
; in: pairing heaps ph1, ph2
; preconditions: ph1, ph2 are min-PHs
; out: min-PH resulting from union(ph1, ph2)
; RESTRICTIONS:
;  - The definition must be point-free.
(define (comp-min ph1 ph2)
  (- (ph-root ph1) (ph-root ph2)))

(define merge-min (merge-f comp-min))

; merge-max-rating : PH x PH -> PH
; in: pairing heaps ph1, ph2
; preconditions: ph1, ph2 contain point pairs
; (name . rating) and are ordered max-PHs
; by rating
; out: max-PH resulting from union(ph1, ph2)
; RESTRICTIONS:
;  - The definition must be point-free.
(define (comp-max-rating ph1 ph2)
  (- (cdr (ph-root ph2)) (cdr (ph-root ph1))))

(define merge-max-rating (merge-f comp-max-rating))


; TODO 2
; Redefine the following functions from step 1 that
; call (directly or indirectly) merge, so that
; the merge function is given as a parameter

;  - ph-insert
(define (ph-insert f val ph)
  (f ph (val->ph val)))

;  - list->ph
(define (list->ph f lst)
  (if (null? lst)
      empty-ph
      (ph-insert f (car lst) (list->ph f (cdr lst)))))

;  - two-pass-merge-LR
(define (two-pass-merge-LR-helper f phs acc)
  (cond
    ((ph-empty? phs) acc)
    ((= (length phs) 1) (f acc (car phs)))
    (else (two-pass-merge-LR-helper f (cddr phs) (f acc (f (car phs) (cadr phs)))))))

(define (two-pass-merge-LR f phs)
  (two-pass-merge-LR-helper f phs null))

;  - ph-del-root
(define (ph-del-root f ph)
  (if (ph-empty? ph)
      #f
      (two-pass-merge-LR f (ph-subtrees ph))))


;; PART TWO (the one where we process films)

;; We define a movie as a structure with 5 fields:
;; name, rating, genre, duration, others.
(define-struct movie (name rating genre duration others) #:transparent)


; TODO 3
; lst->movie : [Symbol, Number, Symbol, [Int], [Symbol]] -> Movie
; in: lst list with 5 values, in this order:
;     - name represented as a symbol (ex: 'the-lives-of-others)
;     - rating represented as a number (ex: 8.4)
;     - genre represented as a symbol (ex: 'drama)
;     - duration represented as a list of hours and minutes (ex: '(2 17))
;     - others represented as a list of symbols (ex: '(german))
; out: movie object instantiated with the 5 values
; RESTRICTIONS:
;  - Do not identify the elements of the list, but use a functional.
(define (lst->movie lst)
  (apply movie lst))


; TODO 4
; mark-as-seen : Movie -> Movie
; in: movie m
; out: m updated so that the symbol 'seen is
;      added to the beginning of the others field (list)
(define (mark-as-seen m)
  (struct-copy movie m [others (append '(seen) (movie-others m))]))


; TODO 5
; mark-as-seen-from-list : [Movie] x [Symbol] -> [Movie]
; in: movie list movies, name list seen
; out: movie list updated so that movies
;      with names in the seen list are marked as seen
; RESTRICTIONS:
;  - Do not use explicit recursion.
;  - Use at least one functional.
(define (is-movie-in-list movie-name seen)
  (if (member movie-name seen)
      #t
      #f))

(define (mark-as-seen-from-list movies seen)
  (map (lambda (x) (if (is-movie-in-list (movie-name x) seen) (mark-as-seen x) x)) movies))

 
; TODO 6
; extract-seen : [Movie] -> [Symbol]
; in: movie list movies
; out: list of movie names seen from the movies list
;      (seen = others contains 'seen)
; RESTRICTIONS:
;  - Do not use explicit recursion.
;  - Do not use fold functional.
;  - Use at least one functional.
(define (is-movie-seen m)
  (if (member 'seen (movie-others m))
      #t
      #f))

(define (extract-seen movies)
  (map movie-name (filter is-movie-seen movies)))


; TODO 7
; rating-stats : [Movie] -> (Number, Number)
; in: movie list movies
; out: pair (rating-average-seen . rating-average-unseen)
;  - rating-average-seen = average of ratings of seen movies
;  - the same for unseen and unseen movies
; (if there are no movies of a certain kind, the average is 0)
; RESTRICTIONS
;  - Do not use explicit recursion.
;  - Use at least one functional.
;  - Do not iterate over the movies in the list (or parts of the list)
;    more than once.
(define (get-rating-stats-pairs movies)
  (foldl (lambda (x acc)
           (if (is-movie-seen x)
               (cons
                 (cons (+ (movie-rating x) (caar acc)) (add1 (cdar acc)))
                 (cdr acc))
               (cons
                 (car acc)
                 (cons (+ (movie-rating x) (cadr acc)) (add1 (cddr acc))))))
         (cons (cons 0 0) (cons 0 0))
         movies))

(define (rating-stats movies)
  (define ratings-pair (get-rating-stats-pairs movies))
  (cond
    ((and (zero? (cdar ratings-pair)) (zero? (cddr ratings-pair))) (cons 0 0))
    ((zero? (cdar ratings-pair)) (cons 0 (/ (cadr ratings-pair) (cddr ratings-pair))))
    ((zero? (cddr ratings-pair)) (cons (/ (caar ratings-pair) (cdar ratings-pair)) 0))
    (else (cons (/ (caar ratings-pair) (cdar ratings-pair)) (/ (cadr ratings-pair) (cddr ratings-pair))))))


; TODO 8
; extract-name-rating : [Movie] -> [(Symbol, Number)]
; in: movie list movies
; out: list of pairs (name . rating)
;      (one pair for each movie in movies)
; RESTRICTIONS:
;  - Do not use explicit recursion.
;  - Use at least one functional.
(define (extract-name-rating movies)
  (map (lambda (x) (cons (movie-name x) (movie-rating x))) movies))


; TODO 9
; make-rating-ph : [Movie] -> PH
; in: movie list movies
; out: max-PH containing pairs (name . rating)
;      corresponding to the movies in movies
;      (sorted by rating)
;  - insert the last pair into the empty PH
;  - ...
;  - insert the first pair into the current PH
(define (make-rating-ph movies)
  (list->ph merge-max-rating (extract-name-rating movies)))


; TODO 10
; before? : T1 x T2 x List
;           (List is a heterogeneous list)
; in: any values ​​a, b, any list List
; out: true, if a = b or a appears before b in List
;      false, otherwise
; RESTRICTIONS:
;  - Do not use explicit recursion.
;  - Identify the findf function in the Help Desk
;    and use it.
(define (before? a b L)
  (cond
    ((equal? a b) #t)
    ((equal? (findf (lambda (x) (or (equal? x a) (equal? x b))) L) a) #t)
    (else #f)))


; TODO 11
; make-genre-ph : [Movie] x [Symbol] -> PH
; in: movie list movies, genre list genres
; out: PH containing movies, so that the genre
;      of a parent node appears in the list genres
;      before the genre of its children
;      (as defined in the function before?)
;  - insert the last movie into the empty PH
;  - ...
;  - insert the first movie into the PH so far
; note: when inserting a movie of the same genre as
;       the current root, the new movie becomes the
;       root's child
(define (comp-genres genres)
  (lambda (ph1 ph2)
    (if (before? (movie-genre (car ph1)) (movie-genre (car ph2)) genres)
        -1
        1)))

(define (merge-by-genres genres) (merge-f (comp-genres genres)))

(define (make-genre-ph movies genres)
  (list->ph (merge-by-genres genres) movies))
