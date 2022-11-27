#lang racket


;; Code here
(require math/array)
(require math/matrix)


(require racket/generator)


(define table
  (generator ()
    (for
      ([line (in-lines)])
      (let ([xy (string-split line ";" #:trim? #t #:repeat? 2)])
        (yield
          (cons
            (string->number (first xy))
            (string->number (second xy))))))))


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
  (with-handlers
    ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let*
      ([coefficients (linear-coefficients N SX SXX SY SXY)]
      [a (car coefficients)]
      [b (cdr coefficients)])
      (lambda (x) (+ (* a x) b)))))


(define (quadratic N SX SXX SY SXY SXXX SXXXX SXXY)
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
      (lambda (x) (+ a0 (* a1 x) (* a2 x x))))))


(define (exponential N SX SXX SLnY SXLnY)
  (with-handlers
    ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let*
      ([coefficients (linear-coefficients N SX SXX SLnY SXLnY)]
       [A (first coefficients)]
       [B (second coefficients)]
       [a (exp B)]
       [b A])
      (lambda (x) (* a (exp (* b x)))))))


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
(define frequency (make-parameter 1))

(command-line
  #:program "approximate"
  #:once-each
  [("-l" "--linear") "Use linear approximation"
                      (linear-enabled #t)]
  [("-q" "--quadratic") "Use quadratic approximation"
                        (quad-enabled #t)]
  [("-e" "--exponent") "Use exponential approximation"
                      (exp-enabled #t)]
  [("-g" "--logarithm") "Use logarithmic approximation"
                        (log-enabled #t)]
  [("-p" "--power") "Use power approximation"
                    (pow-enabled #t)]
  [("-s" "--segment") "Use segment approximation"
                      (seg-enabled #t)]
  [("--step") raw-step "Step between yielded X, default 1"
                        (step (string->number raw-step))]
  [("-f" "--frequency") raw-freq "the number of generated X, default 1"
                                 (frequency (string->number 1))]
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
  (for/fold
    ([function null] #:result (void))
    ([xy (in-producer table (void))])
    (set! function (append function (list xy)))
    (let*
      ([N (length function)]
       [x (car xy)]
       [y (cdr xy)]
       [Xs    (map car function)]
       [Ys    (map cdr function)]
       [SX    (apply + Xs)]
       [SXX   (apply + (map (lambda (x) (expt x 2)) Xs))]
       [SXXX  (apply + (map (lambda (x) (expt x 3)) Xs))]
       [SXXXX (apply + (map (lambda (x) (expt x 4)) Xs))]
       [LnXs  (map (lambda (x) (log x)) Xs)]
       [SLnX  (apply + LnXs)]
       [SY    (apply + Ys)]
       [LnYs  (map (lambda (y) (log y)) Ys)]
       [SLnY  (apply + LnYs)]
       [SXLnY (apply + (map (lambda (x lny) (* x lny)) Xs LnYs))]
       [SXY   (apply + (map (lambda (x y) (* x y)) Xs Ys))]
       [SXXY  (apply + (map (lambda (x y) (* x x y)) Xs Ys))])
      (printf "~a" x)
      (when (linear-enabled) (printf ";~a" ((linear N SX SXX SY SXY) x)))
      (when (quad-enabled)   (printf ";~a" ((quadratic N SX SXX SY SXY SXXX SXXXX SXXY) x)))
      (when (exp-enabled)    (printf ";~a" ((exponential N SX SXX SLnY SXLnY) x)))
      (when (log-enabled)    (printf ";~a" y))
      (when (pow-enabled)    (printf ";~a" y))
      (when (seg-enabled)    (printf ";~a" y))
      (newline)
      function)))
