#lang racket


;; Code here
(require math/array)
(require math/matrix)


(require racket/stream)
(require racket/generator)


(define table
  (for/stream
    ([line (in-lines)])
    (let ([xy (string-split line ";" #:trim? #t #:repeat? 2)])
      (cons
        (string->number (first xy))
        (string->number (second xy))))))


(define (linear-coefficients N SX SXX SY SXY)
  (let*
    ([delta (lambda (arg1 arg2 arg3 arg4) (- (* arg1 arg2) (* arg3 arg4)))]
     [delta0 (delta SXX N SX SX)]
     [delta1 (delta SXY N SX SY)]
     [delta2 (delta SXX SY SX SXY)]
     [a (/ delta1 delta0)]
     [b (/ delta2 delta0)])
    (cons a b)))


(define (linear N SX SXX SY SXY)
  (let*
    ([coefficients (linear-coefficients N SX SXX SY SXY)]
    [a (car coefficients)]
    [b (cdr coefficients)])
    (lambda (x) (+ (* a x) b))))


(define (quadratic N SX SXX SY SXY SXXX SXXXX SXXY)
  (let*
    ([coefficients
      (array->list
        (matrix-solve
          (matrix [[N SX SXX] [SX SXX SXXX] [SXX SXXX SXXXX]])
          (col-matrix [SY SXY SXXY])))]
    [a0 (first coefficients)]
    [a1 (second coefficients)]
    [a2 (third coefficients)])
    (lambda (x) (+ a0 (* a1 x) (* a2 x x)))))


(define (exponential N SX SXX SLnY SXLnY)
  (let*
    ([coefficients (linear-coefficients N SX SXX SLnY SXLnY)]
      [A (car coefficients)]
      [B (cdr coefficients)]
      [a (exp B)]
      [b A])
    (lambda (x) (* a (exp (* b x))))))


(define (logarithmic N SLnX SLnX2 SY SLnXY)
  (let*
    ([coefficients (linear-coefficients N SLnX SLnX2 SY SLnXY)]
     [a (car coefficients)]
     [b (cdr coefficients)])
    (lambda (x) (+ (* a (log x)) b))))


(define (power N SLnX SLnX2 SLnY SLnXLnY)
  (let*
    ([coefficients (linear-coefficients N SLnX SLnX2 SLnY SLnXLnY)]
     [A (car coefficients)]
     [B (cdr coefficients)]
     [a (exp B)]
     [b A])
    (lambda (x) (* a (expt x b)))))


(define (segment xs ys)
  (if (or (null? xs) (null? ys))
    (lambda (x) +nan.0)
    (let*
      ([left-bound  (apply min xs)]
       [right-bound (apply max xs)])
      (lambda (x)
        (if (or (< x left-bound) (> x right-bound))
          +nan.0
          (let*
            ([x0
              (argmin
                (lambda (maybe-x) (- x maybe-x))
                (filter-not (lambda (arg) (> arg x)) xs))]
            [y0 (list-ref ys (index-of xs x0))]
            [x1 (last (member x0 xs))]
            [y1 (list-ref ys (index-of xs x1))])
            (if (eq? x0 x1)
              y0
              (+ y0 (* (/ (- y1 y0) (- x1 x0)) (- x x0))))))))))


(define (pretty-print value #:sep [sep ""])
  (define gl-width 15)
  (printf
    "~a~a"
    sep
    (if (rational? value)
      (~r
        #:base 10
        #:precision '(= 4)
        #:notation 'exponential
        #:min-width gl-width
        value)
      (~a
        (if (complex? value) "" value)
        #:align 'right
        #:min-width gl-width))))

(require racket/cmdline)

(define linear-enabled (make-parameter #f))
(define quad-enabled (make-parameter #f))
(define exp-enabled (make-parameter #f))
(define log-enabled (make-parameter #f))
(define pow-enabled (make-parameter #f))
(define seg-enabled (make-parameter #f))
(define start (make-parameter 0))
(define step (make-parameter 1))
(define count (make-parameter +inf.0))


(command-line
  #:program "approximate"
  #:once-each
  [("-l" "--linear")          "Use linear approximation"
                              (linear-enabled #t)]
  [("-q" "--quadratic")       "Use quadratic approximation"
                              (quad-enabled #t)]
  [("-e" "--exponent")        "Use exponential approximation"
                              (exp-enabled #t)]
  [("-g" "--logarithm")       "Use logarithmic approximation"
                              (log-enabled #t)]
  [("-p" "--power")           "Use power approximation"
                              (pow-enabled #t)]
  [("-s" "--segment")         "Use segment approximation"
                              (seg-enabled #t)]
  [("--step") raw-step        "Step between yielded X, default 1"
                              (step (string->number raw-step))]
  [("--start") raw-start      "Starting X, default 0"
                              (start (string->number raw-start))]
  [("-c" "--count") raw-count "Number of approximated x, default +inf.0"
                              (count (string->number raw-count))]
  #:args () (void))


(define (print-header)
  (pretty-print "x")
  (when (linear-enabled) (pretty-print #:sep ";" "linear"))
  (when (quad-enabled)   (pretty-print #:sep ";" "quadratic"))
  (when (exp-enabled)    (pretty-print #:sep ";" "exponent"))
  (when (log-enabled)    (pretty-print #:sep ";" "logarithm"))
  (when (pow-enabled)    (pretty-print #:sep ";" "power"))
  (when (seg-enabled)    (pretty-print #:sep ";" "segment"))
  (newline))


(module+ test
  (require rackunit)
  ;; tests
  (define X (inclusive-range -500.0 500.0 1.0))


  (define linears
    (let
      ([f (lambda (k b) (lambda (x) (+ (* k x) b)))])
      (list
        (f   0  0)
        (f   0  4)
        (f   1  0)
        (f   2  0)
        (f   2  5)
        (f  -1  0)
        (f -11 -7))))


  (define trinominals
    (let
      ([f (lambda (a b c) (lambda (x) (+ (* a x x) (* b x) c)))])

      (list
        (f  0  0 0)
        (f  0  0 3)
        (f  1  0 0)
        (f -1 -3 4))))


  (define exponents
    (let
      ([f (lambda (a b) (lambda (x) (* a (exp (* b x)))))])

      (list
        (f  2  0)
        (f -3  0)
        (f  1  1))))

  (define logarithms
    (let
      ([f (lambda (a b) (lambda (x) (+ (* a (log x)) b)))])

      (list
        (f 0  0)
        (f 0 -1)
        (f 1  0)
        (f 2 -1))))

  (define powers
    (let
      ([f (lambda (a b) (lambda (x) (* a (expt x b))))])
      (list
        (f  1 0)
        (f  2 0)
        (f -1 0))))

  (let*
    ([N     (length X)]
     [SX    (apply + X)]
     [SXX   (apply + (map (lambda (x) (expt x 2)) X))]
     [SXXX  (apply + (map (lambda (x) (expt x 3)) X))]
     [SXXXX (apply + (map (lambda (x) (expt x 4)) X))])

    (for
      ([f linears])
      (let*
        ([Y    (map f X)]
         [SY    (apply + Y)]
         [SXY   (apply + (map (lambda (x y) (* x y)) X Y))]
         [g     (linear N SX SXX SY SXY)]
         [min-x (apply min X)]
         [max-x (apply max X)]
         [h     (segment X Y)])

        (for
          ([x X])
          (check-= (g x) (f x) 1e-05 "Linear approximation doesn't match precision")
          (check-= (h x) (f x) 0.0   "Segment approximation doesn't match exact value"))

        (check-pred nan? (h (- min-x 1.0e-12)) "Nan if outside of area")
        (check-pred nan? (h (+ max-x 1.0e-12)) "Nan if outside of area")))

    (for
      ([f trinominals])
      (let*
        ([Y   (map f X)]
        [SY   (apply + Y)]
        [SXY  (apply + (map (lambda (x y) (* x y)) X Y))]
        [SXXY (apply + (map (lambda (x y) (* x x y)) X Y))]
        [g    (quadratic N SX SXX SY SXY SXXX SXXXX SXXY)])

        (for
          ([x X])
          (check-= (g x) (f x) 1e-05 "Quadratic approximation doesn't match precision"))))

    (for
      ([f exponents])
      (let*
        ([Y     (map f X)]
         [SLnY  (apply + (map (lambda (y) (log y)) Y))]
         [SXLnY (apply + (map (lambda (x y) (* x (log y))) X Y))]
         [g     (exponential N SX SXX SLnY SXLnY)])

        (for
          ([x X])
          (check-= (g x) (f x) 1e-05 "Exponential approximation doesn't match precision"))))

    (for
      ([f logarithms])
      (let*
        ([positive-X (filter positive? X)]
         [Y          (map f positive-X)]
         [N          (length positive-X)]
         [SLnX       (apply + (map (lambda (x) (log x)) positive-X))]
         [SLnX2      (apply + (map (lambda (x) (expt (log x) 2)) positive-X))]
         [SY         (apply + Y)]
         [SLnXY      (apply + (map (lambda (x y) (* (log x) y)) positive-X Y))]
         [g          (logarithmic N SLnX SLnX2 SY SLnXY)])

        (for
          ([x positive-X])
          (check-= (g x) (f x) 1e-05 "Logarithmic approximation doesn't match precision"))))

    (for
      ([f powers])
      (let*
        ([positive-X (filter positive? X)]
         [Y          (map f positive-X)]
         [N          (length positive-X)]
         [SLnX       (apply + (map (lambda (x) (log x)) positive-X))]
         [SLnX2      (apply + (map (lambda (x) (expt (log x) 2)) positive-X))]
         [SLnY       (apply + (map (lambda (y) (log y)) Y))]
         [SLnXLnY    (apply + (map (lambda (x y) (* (log x) (log y))) positive-X Y))]
         [g          (power N SLnX SLnX2 SLnY SLnXLnY)])

        (for
          ([x positive-X])
          (check-= (g x) (f x) 1e-05 "Power approximation doesn't match precision"))))

    (for
      ([f powers])
      (let*
        ([positive-X (filter positive? X)]
         [Y          (map f positive-X)]
         [N          (length positive-X)]
         [SLnX       (apply + (map (lambda (x) (log x)) positive-X))]
         [SLnX2      (apply + (map (lambda (x) (expt (log x) 2)) positive-X))]
         [SLnY       (apply + (map (lambda (y) (log y)) Y))]
         [SLnXLnY    (apply + (map (lambda (x y) (* (log x) (log y))) positive-X Y))]
         [g          (power N SLnX SLnX2 SLnY SLnXLnY)])

        (for
          ([x positive-X])
          (check-= (g x) (f x) 1e-05 "Power approximation doesn't match precision"))))))


(module+ main
  (print-header)
  (let*-values
    ([(more? get) (sequence-generate table)])

    (for/fold

      ;;; accums
      ([N       0]
       [SX      0]
       [SXX     0]
       [SXXX    0]
       [SXXXX   0]
       [SLnX    0]
       [SLnX2   0]
       [SY      0]
       [SLnY    0]
       [SXLnY   0]
       [SLnXY   0]
       [SXY     0]
       [SLnXLnY 0]
       [SXXY    0]
       [Xs   null]
       [Ys   null]
       #:result (void))

      ;;; looped-structure
      ([x0
        (in-generator
          (let loop ([x (start)])
            (begin
              (yield x)
              (loop (+ x (step))))))]

        #:break (>= N (count)))

      (when
        (> N 1)
        ;;; body
        (pretty-print x0)
        (when (linear-enabled) (pretty-print #:sep ";" ((linear N SX SXX SY SXY) x0)))
        (when (quad-enabled)   (pretty-print #:sep ";" ((quadratic N SX SXX SY SXY SXXX SXXXX SXXY) x0)))
        (when (exp-enabled)    (pretty-print #:sep ";" ((exponential N SX SXX SLnY SXLnY) x0)))
        (when (log-enabled)    (pretty-print #:sep ";" ((logarithmic N SLnX SLnX2 SY SLnXY) x0)))
        (when (pow-enabled)    (pretty-print #:sep ";" ((power N SLnX SLnX2 SLnY SLnXLnY) x0)))
        (when (seg-enabled)    (pretty-print #:sep ";" ((segment Xs Ys) x0)))
        (newline))
      (if (more?)
        (let*
          ([xy (get)]
           [x (car xy)]
           [y (cdr xy)])

          (values
            (add1 N)
            (+ SX x)
            (+ SXX (* x x))
            (+ SXXX (expt x 3))
            (+ SXXXX (expt x 4))
            (+ SLnX (log x))
            (+ SLnX2 (expt (log x) 2))
            (+ SY y)
            (+ SLnY (log y))
            (+ SXLnY (* x (log y)))
            (+ SLnXY (* (log x) y))
            (+ SXY (* x y))
            (+ SLnXLnY (* (log x) (log y)))
            (+ SXXY (* x x y))
            (append Xs (list x))
            (append Ys (list y))))
        (values N SX SXX SXXX SXXXX SLnX SLnX2 SY SLnY SXLnY SLnXY SXY SLnXLnY SXXY Xs Ys)))))
