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


(define (linear left-x right-x N SX SXX SY SXY)
  (with-handlers
    ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let*
      ([coefficients (linear-coefficients N SX SXX SY SXY)]
      [a (car coefficients)]
      [b (cdr coefficients)])
      (lambda (x)
        (if (and (<= x right-x) (>= x left-x))
          (+ (* a x) b)
          +nan.0)))))


(define (quadratic left-x right-x N SX SXX SY SXY SXXX SXXXX SXXY)
  (with-handlers
    ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let*
      ([coefficients
        (array->list
          (matrix-solve
            (matrix [[N SX SXX] [SX SXX SXXX] [SXX SXXX SXXXX]])
            (col-matrix [SY SXY SXXY])))]
      [a0 (first coefficients)]
      [a1 (second coefficients)]
      [a2 (third coefficients)])
      (lambda (x)
        (if (and (<= x right-x) (>= x left-x))
          (+ a0 (* a1 x) (* a2 x x))
          +nan.0)))))


(define (exponential left-x right-x N SX SXX SLnY SXLnY)
  (with-handlers
    ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let*
      ([coefficients (linear-coefficients N SX SXX SLnY SXLnY)]
       [A (first coefficients)]
       [B (second coefficients)]
       [a (exp B)]
       [b A])
      (lambda (x)
        (if (and (<= x right-x) (>= x left-x))
          (* a (exp (* b x)))
          +nan.0)))))


(define (logarithmic N SLnX SLnX2 SY SLnXY)
  (let*
    ([coefficients (linear-coefficients N SLnX SLnX2 SY SLnXY)]
     [a (first coefficients)]
     [b (second coefficients)])
    (lambda (x) (+ (* a (log x)) b))))


(define (power N SLnX SLnX2 SLnY SLnXLnY)
  (let*
    ([coefficients (linear-coefficients N SLnX SLnX2 SLnY SLnXLnY)]
     [A (first coefficients)]
     [B (second coefficients)]
     [a (exp B)]
     [b A])
    (lambda (x) (* a (expt x b)))))


(define (segment xs ys)
  (let*
    ([left-bound (apply min xs)]
     [right-bound (apply max xs)])
    (lambda (x)
      (if (or (< x left-bound) (> x right-bound))
        (raise-argument-error 'f (format "between ~a and ~a" left-bound right-bound) x)
        (let*
          ([x0
            (argmin
              (lambda (maybe-x) (- x maybe-x))
              (filter-not (lambda (arg) (> arg x)) xs))]
           [y0 (list-ref ys (index-of xs x0))]
           [x1 (second (member x0 xs))]
           [y1 (list-ref ys (index-of xs x1))])
          (+ y0 (* (/ (- y1 y0) (- x1 x0)) (- x x0))))))))


(require racket/cmdline)

(define linear-enabled (make-parameter #f))
(define quad-enabled (make-parameter #f))
(define exp-enabled (make-parameter #f))
(define log-enabled (make-parameter #f))
(define pow-enabled (make-parameter #f))
(define seg-enabled (make-parameter #f))
(define start (make-parameter 0))
(define step (make-parameter 1))


(command-line
  #:program "approximate"
  #:once-each
  [("-l" "--linear")     "Use linear approximation"
                         (linear-enabled #t)]
  [("-q" "--quadratic")  "Use quadratic approximation"
                         (quad-enabled #t)]
  [("-e" "--exponent")   "Use exponential approximation"
                         (exp-enabled #t)]
  [("-g" "--logarithm")  "Use logarithmic approximation"
                         (log-enabled #t)]
  [("-p" "--power")      "Use power approximation"
                         (pow-enabled #t)]
  [("-s" "--segment")    "Use segment approximation"
                         (seg-enabled #t)]
  [("--step") raw-step   "Step between yielded X, default 1"
                         (step (string->number raw-step))]
  [("--start") raw-start "Starting X, default 0"
                         (start (string->number raw-start))]
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
       [left-x  #f]
       [right-x #f]
       [SX      0]
       [SXX     0]
       [SXXX    0]
       [SXXXX   0]
       [SLnX    0]
       [SY      0]
       [SLnY    0]
       [SXLnY   0]
       [SXY     0]
       [SXXY    0]
       #:result (void))

      ;;; looped-structure
      ([x0
        (in-generator
          (let loop ([x (start)])
            (begin
              (yield x)
              (loop (+ x (step))))))])

      ;;; body
      (printf "~a" x0)
      (when (linear-enabled) (printf ";~a" ((linear left-x right-x N SX SXX SY SXY) x0)))
      (when (quad-enabled)   (printf ";~a" ((quadratic left-x right-x N SX SXX SY SXY SXXX SXXXX SXXY) x0)))
      (when (exp-enabled)    (printf ";~a" ((exponential left-x right-x N SX SXX SLnY SXLnY) x0)))
      (when (log-enabled)    (printf ";~a" 0))
      (when (pow-enabled)    (printf ";~a" 0))
      (when (seg-enabled)    (printf ";~a" 0))
      (newline)
      (if (more?)
        (let*
          ([xy (get)]
           [x (car xy)]
           [y (car xy)])

          (values
            (add1 N)
            (if left-x  (min left-x x)  x)
            (if right-x (max right-x x) x)
            (+ SX x)
            (+ SXX (* x x))
            (+ SXXX (expt x 3))
            (+ SXXXX (expt x 4))
            (+ SLnX (log x))
            (+ SY y)
            (+ SLnY (log y))
            (+ SXLnY (* x (log y)))
            (+ SXY (* x y))
            (+ SXXY (* x x y))))
        (values N left-x right-x SX SXX SXXX SXXXX SLnX SY SLnY SXLnY SXY SXXY)))))
