#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         racket/math
         racket/random)

;;; CONSTANTS AND STRUCTS

;constants
(define W 800) ;canvas width
(define H 800) ;canvas height
(define w 2)   ;pen width
(define n 32)  ;number of sites and colors

(define D 'euclidean) ;distance function ('manhattan for Manhattan distance, 'chebyshev for Chebyshev distance, returns the Euclidean distance on all other values)

;structs
(struct point (x y))
(struct site (pos color))

;;; MATH

;custom Cartesian product procedure (only computes binary Cartesian products and outputs a pair? of any/c instead of a list? to slightly abbreviate our definition of the Euclidean plane)
(define (cartesian-product lst1 lst2)(apply append
                                            (for/list ([i lst1])
                                              (for/list ([j lst2])
                                                (cons i j)))))

;Euclidean plane definition
(define plane (map (λ (x) (point (car x) (cdr x)))
                   (cartesian-product (range 0 W)
                                      (range 0 H))))

;difference procedures
(define (dx a b)(- (point-x a)
                   (point-x b)))

(define (dy a b)(- (point-y a)
                   (point-y b)))

;Euclidean distance procedure
(define (euclidean-distance a b)(sqrt (+ (sqr (dx a b))
                                         (sqr (dy a b)))))

;Manhattan distance procedure
(define (manhattan-distance a b)(+ (abs (dx a b))
                                   (abs (dy a b))))

;Chebyshev distance procedure
(define (chebyshev-distance a b)(max (abs (dx a b))
                                     (abs (dy a b))))

;distance procedure handler
(define (distance a b f)((cond [(eq? f 'manhattan) manhattan-distance]
                               [(eq? f 'chebyshev) chebyshev-distance]
                               [else euclidean-distance])
                         a b))

;nearest neighbor procedure
(define (nearest-neighbor p lst)(argmin (λ (x) (distance p (site-pos x) D)) lst))

;;; RANDOM SITE GENERATOR

;random color procedure
(define (random-color)(make-object color% (random 0 256) (random 0 256) (random 0 256)))

;defines a list of random colors with length n
(define colors (for/list ([i (range n)])
                 (random-color)))

;defines a list of random points with length n
(define points (random-sample plane n #:replacement? #f))

;defines a list of random sites with length n
(define sites (map site points colors))

;;;GRAPHICS

;top level frame
(define f (new frame%
               [label "Random Voronoi Diagram Generator"]
               [width W]
               [height H]))

;canvas
(new canvas%
     [parent f]
     [paint-callback
      (λ (canvas dc)
        (define (draw p)
          (send* dc
            (set-pen (send the-pen-list find-or-create-pen (site-color (nearest-neighbor p sites)) w 'solid))
            (draw-point (point-x p) (point-y p))))
        (for-each draw plane))])

(send f show #t)