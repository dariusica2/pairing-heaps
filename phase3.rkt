#lang racket
(require "etapa2.rkt")
(require racket/match)
(provide (all-defined-out))

;; This phase is dedicated to applications of
;; pairing heaps, which we will use to:
;;  - extract the best movies from a list, according
;;  to a certain criterion
;;  - extract the best reviews from a collection
;;  of reviews for various movies (a good review
;;  corresponds to a good grade given to the respective movie)
;;
;; To successfully complete the stage it is necessary to
;; solve the tasks with the algorithms dedicated to PHs
;; (described in the statement)

; TODO 1
; Define the best-k function in a form that
; facilitates further derivation of the functions
; best-k-rating and best-k-duration.
; in: comparison criterion op (which compares 2 movies),
;     list of movies, number k
; out: sorted list of the "best" movies
;      according to the criterion (the "best" first)
; Algorithm:
;  1. construct a PH of movies based on the list of movies
;     and the criterion op
;  2. repeatedly extract the root of this PH until
;     the result contains k movies (or the PH becomes empty)
; RESTRICTIONS:
;  - Use named let to perform step 2 of
;    the algorithm.

(define (best-k op movies k)
  (let ((sorted-list (list->ph op movies)))
    (let iter ((sorted-list sorted-list) (k k) (result null))
      (if (or (null? sorted-list) (zero? k))
          (reverse result)
          (iter (ph-del-root op sorted-list) (- k 1) (cons (ph-root sorted-list) result))))))

; best-k-rating : [Movie] x Int -> [Movie]
; in: movie list movies, number k
; out: the best k movies from movies (as rating)
; RESTRICTIONS:
;  - Get best-k-rating as an application of best-k.

(define (comp-movies-max-ratings ph1 ph2)
  (- (movie-rating (ph-root ph2)) (movie-rating (ph-root ph1)))) 

(define merge-movies-max-ratings (merge-f comp-movies-max-ratings))

(define (best-k-rating movies k)
  (best-k merge-movies-max-ratings movies k))

; best-k-duration : [Movie] x Int -> [Movie]
; in: movie list movies, number k
; out: the shortest k movies from movies
; RESTRICTIONS:
;  - Get best-k-duration as best-k's app.

(define (comp-movies-min-duration ph1 ph2)
  (let ((duration1 (movie-duration (ph-root ph1))) (duration2 (movie-duration (ph-root ph2))))
    (if (equal? (car duration1) (car duration2))
        (- (cadr duration1) (cadr duration2))
        (- (car duration1) (car duration2)))))

(define merge-movies-min-duration (merge-f comp-movies-min-duration))

(define (best-k-duration movies k)
  (best-k merge-movies-min-duration movies k))

; TODO 2
; update-pairs : ((Symbol, PH) -> Bool) x [(Symbol, PH)]
;                -> [(Symbol, PH)]
; in: predicate p, list of pairs (movie-name . PH)
;     (PH is a max-PH containing the ratings given to
;     the film in various reviews - hence a PH
;     of numbers)
; out: the pairs list updated as follows:
;      - for the first pair that satisfies the predicate
;        p, the PH of the pair is removed
;      - if the PH of the pair is empty or if no pair
;        satisfies p, the pairs list is returned unchanged
; RESTRICTIONS:
;  - Use named let to iterate through the pairs.

(define (pair-del-root-helper pair)
  (if (null? (cdr pair))
      pair
      (cons (car pair) (ph-del-root merge-max (cdr pair)))))

(define (update-pairs p pairs)
  (let iter ((pairs pairs) (result null))
    (cond
      ((null? pairs) (reverse result))
      ((p (car pairs)) (append (reverse result) (cons (pair-del-root-helper (car pairs)) (cdr pairs))))
      (else (iter (cdr pairs) (cons (car pairs) result))))))

; TODO 3
; best-k-ratings-overall : [(Symbol, PH)] x Int
;                          -> [(Symbol, Number)]
; in: list of pairs (movie-name . PH)
;     (as above, PH is a max-PH of ratings)
;     number k
; out: sorted list of the best k pairs
;      (movie-name . rating), corresponding to the best
;      ratings of all PHs
; Algorithm:
;  1. Initialize a PH of pairs (name . rating),
;     corresponding to the best rating of each movie
;     (i.e. extract the root of each PH of ratings,
;     paired with the corresponding movie name)
; 2. Repeat k times:
;    - extract the root of the PH of roots
;      (name-root . rating-root)
;      (i.e. extract the best pair overall)
;    - bring the next best
;      rating of movie name-root into the PH of roots (if any)
; RESTRICTIONS:
; - Use named let to perform step 2 of
;   the algorithm.

(define (initialise-ph pairs)
  (list->ph merge-max-rating (map (lambda (x) (cons (car x) (cadr x))) pairs)))

(define (initialise-pairs pairs)
  (map (lambda (x) (cons (car x) (ph-del-root merge-max (cdr x)))) pairs))

(define (best-k-ratings-overall pairs k)
  (let iter ((max-ph-pair-ratings
             (initialise-ph pairs))
             (pairs-ratings (initialise-pairs pairs))
             (k k)
             (result null))
    (cond
      ((or (zero? k) (ph-empty? max-ph-pair-ratings)) (reverse result))
      (else
       (let ((result-addition (ph-root max-ph-pair-ratings))) ; the pair that will be added to result
         (let ((searched-movie (car result-addition))) ; movie that is searched in 'pairs' list
           (let ((searched-list (findf (lambda (x) (equal? (car x) searched-movie)) pairs-ratings)))
             (if (null? (cdr searched-list)) ; if there are no reviews left for a movie
                 (iter (ph-del-root merge-max-rating max-ph-pair-ratings)
                       (update-pairs (lambda (x) (equal? (car x) searched-movie)) pairs-ratings)
                       (- k 1)
                       (cons result-addition result))
                 (let ((max-ph-pair-addition (cons (car searched-list) (ph-root (cdr searched-list)))))
                   (iter (ph-insert merge-max-rating max-ph-pair-addition (ph-del-root merge-max-rating max-ph-pair-ratings))
                         (update-pairs (lambda (x) (equal? (car x) searched-movie)) pairs-ratings)
                         (- k 1)
                         (cons result-addition result)))))))))))

