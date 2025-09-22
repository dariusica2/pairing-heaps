#lang racket
(require racket/match)
(require "etapa2.rkt")
(provide (all-defined-out))

;; This phase continues the series of applications of the
;; pairing heaps, which we will use to calculate
;; dynamically the median of a movie's reviews, simulating
;; real-world conditions - in which
;; new reviews for various movies constantly appear.
;;
;; To model this dynamic, we use a stream of
;; pairs (movie-name . rating), based on which we calculate
;; a stream of evolutionary stages as follows:
;;  - each stage is represented as a list of pairs
;;   * one pair for each movie with at least one review
;;   * each pair is of the form
;;     (movie-name . median-ratings-received-so-far)
;;  - each new review determines the update of a
;;    median, i.e. the transition to another stage,
;;    generating a new element in the resulting stream
;;
;; The algorithm used is the following:
;;  The stream of pairs is transformed into a stream of
;;  lists of quartets (movie-name delta max-ph min-ph)
;;   - each element in the stream contains a quartet
;;     for each movie that has at least one review
;;   - if the movie has an even number of reviews:
;;   - max-ph and min-ph have the same size
;;   - delta = size(max-ph) - size(min-ph) = 0
;;   - max-ph = max-PH with the lowest ratings
;;   - min-ph = min-PH with the highest ratings
;;   - the median is the average of the roots of the 2 PHs
;;   - if the movie has an odd number of reviews:
;;   - max-ph has one more element than min-ph
;;   - delta = size(max-ph) - size(min-ph) = 1
;;   - max-ph = max-PH with the lowest ratings
;;   - min-ph = min-PH with the highest ratings
;;   - median is the root of max-ph


; TODO 1
; add-rating : (Symbol, Int, PH, PH) x Number
;              -> (Symbol, Int, PH, PH)
; in: quartet (name delta max-ph min-ph),
;     rating to add
; out: quartet updated by adding
;      the rating, like this:
;  - if rating <= root(max-ph)
;    insert rating into max-ph, updating delta
;  - otherwise
;    insert rating into min-ph, updating delta
;  - if delta > 1
;    move root(max-ph) into min-ph
;  - if delta < 0
;    move root(min-ph) to max-ph

(define (add-rating quad rating)
  (let ((name (car quad))
        (delta (cadr quad))
        (max-ph (caddr quad))
        (min-ph (cadddr quad)))
    (if (<= rating (ph-root max-ph))
        (let* ((updated-max-ph (ph-insert merge-max rating max-ph))
               (updated-delta (add1 delta))
               (max-ph-root (ph-root updated-max-ph)))
          (if (> updated-delta 1)
              (list name
                    0
                    (ph-del-root merge-max updated-max-ph)
                    (ph-insert merge-min max-ph-root min-ph))
              (list name
                    updated-delta
                    updated-max-ph
                    min-ph)))
        (let* ((updated-min-ph (ph-insert merge-min rating min-ph))
               (updated-delta (sub1 delta))
               (min-ph-root (ph-root updated-min-ph)))
          (if (< updated-delta 0)
              (list name
                    1
                    (ph-insert merge-max min-ph-root max-ph)
                    (ph-del-root merge-min updated-min-ph))
              (list name
                    updated-delta
                    max-ph
                    updated-min-ph))))))

; TODO 2
; reviews->quads : Stream<(Symbol, Number)> ->
;                  Stream<[(Symbol, Int, PH, PH)]>
; in: stream of pairs (name . rating)
; out: stream of lists of quartets
;      (name delta max-ph min-ph)
;  - the kth element of the result corresponds to the first
;    k reviews in the input (e.g. if the first 10
;    reviews are for 3 distinct movies, the
;    10th element of the result stream contains a
;    list of 3 quartets - one for each movie)
; RESTRICTIONS:
;  - Work with operators on streams, without
;    converting lists to streams or streams to lists.

(define (is-movie-in-quads name existing-quads)
  (if (null? (filter (lambda (x) (equal? name (car x))) existing-quads))
      #f
      #t))
      
(define (reviews->quads reviews)
  (let iter ((reviews reviews) (existing-quads null))
    (if (stream-empty? reviews)
        empty-stream
        (let* ((current-review (stream-first reviews))
               (review-name (car current-review))
               (review-rating (cdr current-review))
               (updated-quads
                (if (is-movie-in-quads review-name existing-quads)
                    (map (lambda (x) (if (equal? review-name (car x))
                                         (add-rating x review-rating)
                                         x))
                         existing-quads)
                    (cons (list review-name 1 (val->ph review-rating) empty-ph) existing-quads))))
          (stream-cons updated-quads (iter (stream-rest reviews) updated-quads))))))
      
; TODO 3
; quads->medians : Stream<[(Symbol, Int, PH, PH)]> ->
;                  Stream<[(Symbol, Number)]>  
; in: stream of lists of quartets (as above)
; out: stream of lists of pairs (movie-name . median)
;  - median is calculated based on PHs in
;    each quartet, according to the algorithm above
; RESTRICTIONS:
;  - Do not use explicit recursion. Use at least
;    one functional on streams.

(define (convert-quad-to-median quad)
  (if (equal? (cadr quad) 1)
      (cons (car quad) (ph-root (caddr quad)))
      (cons (car quad) (/ (+ (ph-root (caddr quad)) (ph-root (cadddr quad))) 2))))

(define (quads->medians quads)
  (if (stream-empty? quads)
      empty-stream
      (stream-map (lambda (x) (map convert-quad-to-median x)) quads)))
