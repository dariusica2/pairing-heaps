Assignment: Pairing Heaps in Racket

This assignment focuses on implementing and using pairing heaps (PH) in Racket through 4 progressive stages.

Stage 1 – Basic Implementation of a Max-PH

Learn the representation of pairing heaps as lists in Racket.

Implement a max pairing heap and its fundamental operations:

empty-ph

(val->ph val)

(ph-empty? ph)

(ph-root ph)

(ph-subtrees ph)

(merge ph1 ph2)

(ph-insert val ph)

(list->ph lst)

(two-pass-merge-LR phs)

(two-pass-merge-RL phs)

(tournament-merge phs)

(ph-del-root ph)

Goal: practice lists, recursion (stack and tail), conditionals, and booleans.

⚠️ Penalties:

Not following the required recursion type: -10p/function.

Using list operators instead of PH interface: not penalized, but decreases readability.

📌 Stage 2 – Abstraction and Movies

Generalize PH operations to support min-heaps, max-heaps, and custom orderings:

merge-f – general merging function.

merge-max, merge-min – derived via currying.

Define a movie structure with fields:

name, rating, genre, duration, others.

Implement movie-related functions:

(lst->movie lst)

(mark-as-seen m)

(mark-as-seen-from-list movies seen)

(extract-seen movies)

(rating-stats movies)

(extract-name-rating movies)

(make-rating-ph movies)

(before? a b L)

(make-genre-ph movies genres)

Goal: strengthen knowledge of higher-order functions, anonymous functions, currying, and partial application.

⚠️ Penalties:

Missing point-free definitions for merge-max, merge-min, etc.: -5p/function.

Using explicit recursion instead of higher-order functions where required: -10p/function.

📌 Stage 3 – Advanced Applications

Apply PHs to:

Extract the best k elements by a given criterion.

Merge values across multiple PHs.

Functions:

(best-k op movies k) – general version.

(best-k-rating movies k)

(best-k-duration movies k)

(update-pairs p pairs)

(best-k-ratings-overall pairs k)

Goal: use let, let*, and named let for ad-hoc recursion.

⚠️ Penalties:

best-k-rating, best-k-duration not defined as applications of best-k: -5p/function.

Missing named let where required: -20p/function.

📌 Stage 4 – Dynamic Median and Streams

Compute the dynamic median of movie ratings using:

a max-PH for the smaller half,

a min-PH for the larger half,

constant rebalancing between the two.

Introduce streams:

Inputs are streams of reviews (movie-name . rating).

Outputs are streams of states/medians.

Functions:

(add-rating quad rating)

(reviews->quads reviews)

(quads->medians quads)

Goal: work with potentially infinite streams, modeling temporal evolution.

✅ Summary

This assignment covers:

Implementation of functional data structures in Racket.

Abstraction and reusable design.

Use of higher-order functions, currying, let/named let.

Data processing in infinite streams.