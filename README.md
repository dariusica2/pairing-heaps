# Assignment: Pairing Heaps in Racket

This assignment focuses on implementing and using pairing heaps (PH) in *Racket* through 4 progressive phases.

## Stage 1 – Basic Implementation of a Max-PH

This stage covers:

- Representation of pairing heaps as lists in Racket.

- Implementing a max pairing heap and its fundamental operations:

  - empty-ph

  - (val->ph val)

  - (ph-empty? ph)

  - (ph-root ph)

  - (ph-subtrees ph)

  - (merge ph1 ph2)

  - (ph-insert val ph)

  - (list->ph lst)

  - (two-pass-merge-LR phs)

  - (two-pass-merge-RL phs)

  - (tournament-merge phs)

  - (ph-del-root ph)

Main goals are practicing the use of lists, recursion (stack and tail), conditionals and booleans.

## Stage 2 – Abstraction and Movies

This stage covers:

- Generalization of PH operations to support min-heaps, max-heaps, and custom orderings:

  - merge-f – general merging function

  - merge-max, merge-min – derived via currying

- Defining a movie structure with fields:

  - name

  - rating

  - genre

  - duration

  - others

- Implementing movie-related functions:

  - (lst->movie lst)

  - (mark-as-seen m)

  - (mark-as-seen-from-list movies seen)

  - (extract-seen movies)

  - (rating-stats movies)

  - (extract-name-rating movies)

  - (make-rating-ph movies)

  - (before? a b L)

  - (make-genre-ph movies genres)

Main goals are strengthening knowledge of higher-order functions, currying, and partial application.

## Stage 3 – Advanced Applications

This stage covers:

- Applying PHs to:

  - Extract the best k elements by a given criterion.

  - Merge values across multiple PHs.

- Implementeing the following functions:

  - (best-k op movies k) – general version

  - (best-k-rating movies k)

  - (best-k-duration movies k)

  - (update-pairs p pairs)

  - (best-k-ratings-overall pairs k)

Main goals are using let, let*, and named let for ad-hoc recursion.

## Stage 4 – Dynamic Median and Streams

This stage covers:

- Computing the dynamic median of movie ratings using:

  - a max-PH for the smaller half

  - a min-PH for the larger half

  - constant rebalancing between the two
  
- The introduction of streams:

  - Inputs are streams of reviews (movie-name . rating).

  - Outputs are streams of states/medians.

- Implementing the following functions:

  - (add-rating quad rating)

  - (reviews->quads reviews)

  - (quads->medians quads)

Main goals are working with potentially infinite streams, modeling temporal evolution.

## In summary...

This assignment covers:

- Implementation of functional data structures in Racket.

- Abstraction and reusable design.

- Use of higher-order functions, currying, let/named let.

- Data processing in infinite streams.