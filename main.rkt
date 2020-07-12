#lang racket/base
(require racket/class
         racket/gui/base
         racket/list
         racket/math
         racket/random)

;;; CONSTANTS AND STRUCTURE TYPES

;constants
(define W 800) ;canvas width
(define H 800) ;canvas height
(define w 2)   ;pen width
(define n 32)  ;number of sites and colors

;structure type definitions
(struct point (x y))
(struct site (pos color))

;;; MATH

;custom Cartesian product procedure (only computes binary Cartesian products and outputs a pair? of any/c instead of a list? to slightly abbreviate our definition of the Euclidean plane)
(define (cartesian-product lst1 lst2)(apply append (for/list ([i lst1])
                                                     (for/list ([j lst2])
                                                       (cons i j)))))

;Euclidean plane definition
(define plane (map (λ (x) (point (car x) (cdr x)))
                   (cartesian-product (range 0 W)
                                      (range 0 H))))

;Euclidean distance procedure
(define (euclidean-distance a b)(sqrt (+ (sqr (- (point-x b)
                                                 (point-x a)))
                                         (sqr (- (point-y b)
                                                 (point-y a))))))

;Manhattan distance procedure
(define (manhattan-distance a b)(+ (abs (- (point-x a)
                                           (point-x b)))
                                   (abs (- (point-y a)
                                           (point-y b)))))

;Chebyshev distance procedure
(define (chebyshev-distance a b)(max (abs (- (point-x a)
                                             (point-x b)))
                                     (abs (- (point-y a)
                                             (point-y b)))))

;distance procedure alias (use it to toggle distance functions)
(define distance euclidean-distance)

;nearest neighbor procedure
(define (nearest-neighbor p lst)(argmin (λ (x) (distance p (site-pos x))) lst))

;;; RANDOM SITE GENERATOR

;random color generator (eats a list whose identifier has no bound occurrences in the procedure's body, in order to satisfy the function contract of map)
(define (random-colors lst)(make-object color% (random 0 256) (random 0 256) (random 0 256)))

;defines a list of random sites with length n
(define sites (map site (random-sample plane n #:replacement? #f) (map random-colors (range n))))

;;;GRAPHICS

(define frame (new frame%
                   [label "Random Voronoi Diagram Generator"]
                   [width W]
                   [height H]))
(new canvas% [parent frame]
     [paint-callback
      (λ (canvas dc)
        (define (draw p)
          (send* dc
            (set-pen (send the-pen-list find-or-create-pen (site-color (nearest-neighbor p sites)) w 'solid))
            (draw-point (point-x p) (point-y p))))
        (for-each draw plane))])
(send frame show #t)