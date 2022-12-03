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
      ([left-bound (apply min xs)]
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
            [x1 (second (member x0 xs))]
            [y1 (list-ref ys (index-of xs x1))])
            (+ y0 (* (/ (- y1 y0) (- x1 x0)) (- x x0)))))))))


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
  (printf "x")
  (when (linear-enabled) (printf ";linear"))
  (when (quad-enabled)   (printf ";quadratic"))
  (when (exp-enabled)    (printf ";exponent"))
  (when (log-enabled)    (printf ";logarithm"))
  (when (pow-enabled)    (printf ";power"))
  (when (seg-enabled)    (printf ";segment"))
  (newline))


(module+ test
  (require rackunit)
  ;; tests
  (check-equal? 1 1))


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
        (printf "~a" x0)
        (when (linear-enabled) (printf ";~a" ((linear N SX SXX SY SXY) x0)))
        (when (quad-enabled)   (printf ";~a" ((quadratic N SX SXX SY SXY SXXX SXXXX SXXY) x0)))
        (when (exp-enabled)    (printf ";~a" ((exponential N SX SXX SLnY SXLnY) x0)))
        (when (log-enabled)    (printf ";~a" ((logarithmic N SLnX SLnX2 SY SLnXY) x0)))
        (when (pow-enabled)    (printf ";~a" ((power N SLnX SLnX2 SLnY SLnXLnY) x0)))
        (when (seg-enabled)    (printf ";~a" ((segment Xs Ys) x0)))
        (newline))
      (if (more?)
        (let*
          ([xy (get)]
           [x (car xy)]
           [y (car xy)])

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
