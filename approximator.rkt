#lang racket
#|review: ignore|#

;; Code here
(require math/array)
(require math/matrix)

(require racket/stream)
(require racket/generator)

(define table
  (for/stream ([line (in-lines)])
              (let ([xy (string-split line ";" #:trim? #t #:repeat? 2)])
                (cons (string->number (first xy)) (string->number (second xy))))))

(define (linear-coefficients N SX SXX SY SXY)
  (let* ([delta (lambda (arg1 arg2 arg3 arg4) (- (* arg1 arg2) (* arg3 arg4)))]
         [delta0 (delta SXX N SX SX)]
         [delta1 (delta SXY N SX SY)]
         [delta2 (delta SXX SY SX SXY)]
         [a (/ delta1 delta0)]
         [b (/ delta2 delta0)])
    (cons a b)))

(define (linear N SX SXX SY SXY)
  (let* ([coefficients (linear-coefficients N SX SXX SY SXY)]
         [a (car coefficients)]
         [b (cdr coefficients)])
    (lambda (x) (+ (* a x) b))))

(define (quadratic N SX SXX SY SXY SXXX SXXXX SXXY)
  (with-handlers ([exn:fail? (lambda (e) (lambda (x) +nan.0))])
    (let* ([coefficients (array->list (matrix-solve
                                       (matrix [[N SX SXX] [SX SXX SXXX] [SXX SXXX SXXXX]])
                                       (col-matrix [SY SXY SXXY])))]
           [a0 (first coefficients)]
           [a1 (second coefficients)]
           [a2 (third coefficients)])
      (lambda (x) (+ a0 (* a1 x) (* a2 x x))))))

(define (exponential N SX SXX SLnY SXLnY)
  (let* ([coefficients (linear-coefficients N SX SXX SLnY SXLnY)]
         [A (car coefficients)]
         [B (cdr coefficients)]
         [a (exp B)]
         [b A])
    (lambda (x) (* a (exp (* b x))))))

(define (logarithmic N SLnX SLnX2 SY SLnXY)
  (let* ([coefficients (linear-coefficients N SLnX SLnX2 SY SLnXY)]
         [a (car coefficients)]
         [b (cdr coefficients)])
    (lambda (x) (if (> x 0) (+ (* a (log x)) b) +nan.0))))

(define (power N SLnX SLnX2 SLnY SLnXLnY)
  (let* ([coefficients (linear-coefficients N SLnX SLnX2 SLnY SLnXLnY)]
         [A (car coefficients)]
         [B (cdr coefficients)]
         [a (exp B)]
         [b A])
    (lambda (x) (if (zero? x) +nan.0 (* a (expt x b))))))

(define (segment xs ys)
  (if (or (null? xs) (null? ys))
      (lambda (x) +nan.0)
      (let* ([left-bound (apply min xs)] [right-bound (apply max xs)])
        (lambda (x)
          (if (or (< x left-bound) (> x right-bound))
              +nan.0
              (let* ([x0 (argmin (lambda (maybe-x) (- x maybe-x))
                                 (filter-not (lambda (arg) (> arg x)) xs))]
                     [y0 (list-ref ys (index-of xs x0))]
                     [x1 (last (member x0 xs))]
                     [y1 (list-ref ys (index-of xs x1))])
                (if (eq? x0 x1) y0 (+ y0 (* (/ (- y1 y0) (- x1 x0)) (- x x0))))))))))

(define (make-sums Xs Ys)
  (let* ([N (length Xs)]
         [SX (apply + Xs)]
         [SXX (apply + (map sqr Xs))]
         [SXXX (apply + (map (lambda (x) (expt x 3)) Xs))]
         [SXXXX (apply + (map (lambda (x) (expt x 4)) Xs))]
         [SLnX (apply + (map (lambda (x) (log x)) Xs))]
         [SLnX2 (apply + (map (lambda (x) (sqr (log x))) Xs))]
         [SY (apply + Ys)]
         [SLnY (apply + (map (lambda (y) (log y)) Ys))]
         [SXLnY (apply + (map (lambda (x y) (* x (log y))) Xs Ys))]
         [SLnXY (apply + (map (lambda (x y) (* (log x) y)) Xs Ys))]
         [SXY (apply + (map * Xs Ys))]
         [SLnXLnY (apply + (map (lambda (x y) (* (log x) (log y))) Xs Ys))]
         [SXXY (apply + (map (lambda (x y) (* x x y)) Xs Ys))])
    (list N SX SXX SXXX SXXXX SLnX SLnX2 SY SLnY SXLnY SLnXY SXY SLnXLnY SXXY)))

(define (pretty-print value #:sep [sep ""])
  (define gl-width 15)
  (printf
   "~a~a"
   sep
   (if (rational? value)
       (~a #:align 'right
           #:min-width gl-width
           (~r #:base 10 #:precision '(= 4) #:notation 'exponential #:format-exponent "E" value))
       (~a (if (complex? value) "" value) #:align 'right #:min-width gl-width))))

(require racket/cmdline)

(struct context
        ([sums #:mutable] [xs #:mutable]
                          [ys #:mutable]
                          [flags #:mutable]
                          [start #:mutable]
                          [step #:mutable]
                          [count #:mutable])
  #:transparent)

(define initial-global-context (context (make-list 14 0) null null (make-list 6 #f) 0 1 +inf.0))

(define (next-y ctx x)
  (match-let
      ([(list N SX SXX SXXX SXXXX SLnX SLnX2 SY SLnY SXLnY SLnXY SXY SLnXLnY SXXY) (context-sums ctx)]
       [(list linear? quadratic? exponential? logarithmic? power? segment?) (context-flags ctx)])

    (let ([Xs (context-xs ctx)] [Ys (context-ys ctx)] [y null])

      (when linear?
        (set! y (append y (list ((linear N SX SXX SY SXY) x)))))

      (when quadratic?
        (set! y (append y (list ((quadratic N SX SXX SY SXY SXXX SXXXX SXXY) x)))))

      (when exponential?
        (set! y (append y (list ((exponential N SX SXX SLnY SXLnY) x)))))

      (when logarithmic?
        (set! y (append y (list ((logarithmic N SLnX SLnX2 SY SLnXY) x)))))

      (when power?
        (set! y (append y (list ((power N SLnX SLnX2 SLnY SLnXLnY) x)))))

      (when segment?
        (set! y (append y (list ((segment Xs Ys) x)))))
      y)))

(command-line
 #:program "approximate"
 #:once-each [("-l" "--linear")
              "Use linear approximation"
              (set-context-flags! initial-global-context
                                  (list-set (context-flags initial-global-context) 0 #t))]
 [("-q" "--quadratic")
  "Use quadratic approximation"
  (set-context-flags! initial-global-context (list-set (context-flags initial-global-context) 1 #t))]
 [("-e" "--exponent")
  "Use exponential approximation"
  (set-context-flags! initial-global-context (list-set (context-flags initial-global-context) 2 #t))]
 [("-g" "--logarithm")
  "Use logarithmic approximation"
  (set-context-flags! initial-global-context (list-set (context-flags initial-global-context) 3 #t))]
 [("-p" "--power")
  "Use power approximation"
  (set-context-flags! initial-global-context (list-set (context-flags initial-global-context) 4 #t))]
 [("-s" "--segment")
  "Use segment approximation"
  (set-context-flags! initial-global-context (list-set (context-flags initial-global-context) 5 #t))]
 [("--step")
  raw-step
  "Step between yielded X, default 1"
  (set-context-step! initial-global-context (string->number raw-step))]
 [("--start")
  raw-start
  "Starting X, default 0"
  (set-context-start! initial-global-context (string->number raw-start))]
 [("-c" "--count")
  raw-count
  "Number of approximated x, default +inf.0"
  (set-context-count! initial-global-context (string->number raw-count))]
 #:args ()
 (void))

(define (print-header ctx)
  (pretty-print "x")
  (when (first (context-flags ctx))
    (pretty-print #:sep ";" "linear"))
  (when (second (context-flags ctx))
    (pretty-print #:sep ";" "quadratic"))
  (when (third (context-flags ctx))
    (pretty-print #:sep ";" "exponent"))
  (when (fourth (context-flags ctx))
    (pretty-print #:sep ";" "logarithm"))
  (when (fifth (context-flags ctx))
    (pretty-print #:sep ";" "power"))
  (when (sixth (context-flags ctx))
    (pretty-print #:sep ";" "segment"))
  (newline))

(module+ test
  (require rackunit
           rackcheck)
  ;; tests
  (define X (inclusive-range -500.0 500.0 1.0))

  (define linears
    (let ([f (lambda (k b) (lambda (x) (+ (* k x) b)))])
      (list (f 0 0) (f 0 4) (f 1 0) (f 2 0) (f 2 5) (f -1 0) (f -11 -7))))

  (define trinominals
    (let ([f (lambda (a b c) (lambda (x) (+ (* a x x) (* b x) c)))])

      (list (f 0 0 0) (f 0 0 3) (f 1 0 0) (f -1 -3 4))))

  (define exponents
    (let ([f (lambda (a b) (lambda (x) (* a (exp (* b x)))))])

      (list (f 2 0) (f -3 0) (f 1 1))))

  (define logarithms
    (let ([f (lambda (a b) (lambda (x) (+ (* a (log x)) b)))])

      (list (f 0 0) (f 0 -1) (f 1 0) (f 2 -1))))

  (define powers
    (let ([f (lambda (a b) (lambda (x) (* a (expt x b))))])
      (list (f 1 0) (f 2 0) (f -1 0) (f 1 0.5) (f 1 1) (f 1 2) (f 2 3))))

  (let* ([N (length X)]
         [SX (apply + X)]
         [SXX (apply + (map (lambda (x) (expt x 2)) X))]
         [SXXX (apply + (map (lambda (x) (expt x 3)) X))]
         [SXXXX (apply + (map (lambda (x) (expt x 4)) X))])

    (for ([f linears])
      (let* ([Y (map f X)]
             [SY (apply + Y)]
             [SXY (apply + (map (lambda (x y) (* x y)) X Y))]
             [g (linear N SX SXX SY SXY)]
             [min-x (apply min X)]
             [max-x (apply max X)]
             [h (segment X Y)])

        (for ([x X])
          (check-= (g x) (f x) 1e-05 "Linear approximation doesn't match precision")
          (check-= (h x) (f x) 0.0 "Segment approximation doesn't match exact value"))

        (check-pred nan? (h (- min-x 1.0e-12)) "Nan if outside of area")
        (check-pred nan? (h (+ max-x 1.0e-12)) "Nan if outside of area")))

    (for ([f trinominals])
      (let* ([Y (map f X)]
             [SY (apply + Y)]
             [SXY (apply + (map (lambda (x y) (* x y)) X Y))]
             [SXXY (apply + (map (lambda (x y) (* x x y)) X Y))]
             [g (quadratic N SX SXX SY SXY SXXX SXXXX SXXY)])

        (for ([x X])
          (check-= (g x) (f x) 1e-05 "Quadratic approximation doesn't match precision"))))

    (for ([f exponents])
      (let* ([Y (map f X)]
             [SLnY (apply + (map (lambda (y) (log y)) Y))]
             [SXLnY (apply + (map (lambda (x y) (* x (log y))) X Y))]
             [g (exponential N SX SXX SLnY SXLnY)])

        (for ([x X])
          (check-= (g x) (f x) 1e-05 "Exponential approximation doesn't match precision"))))

    (for ([f logarithms])
      (let* ([positive-X (filter positive? X)]
             [Y (map f positive-X)]
             [N (length positive-X)]
             [SLnX (apply + (map (lambda (x) (log x)) positive-X))]
             [SLnX2 (apply + (map (lambda (x) (expt (log x) 2)) positive-X))]
             [SY (apply + Y)]
             [SLnXY (apply + (map (lambda (x y) (* (log x) y)) positive-X Y))]
             [g (logarithmic N SLnX SLnX2 SY SLnXY)])

        (for ([x positive-X])
          (check-= (g x) (f x) 1e-05 "Logarithmic approximation doesn't match precision"))))

    (for ([f powers])
      (let* ([positive-X (filter positive? X)]
             [Y (map f positive-X)]
             [N (length positive-X)]
             [SLnX (apply + (map (lambda (x) (log x)) positive-X))]
             [SLnX2 (apply + (map (lambda (x) (expt (log x) 2)) positive-X))]
             [SLnY (apply + (map (lambda (y) (log y)) Y))]
             [SLnXLnY (apply + (map (lambda (x y) (* (log x) (log y))) positive-X Y))]
             [g (power N SLnX SLnX2 SLnY SLnXLnY)])

        (for ([x positive-X])
          (check-= (g x) (f x) 1e-04 "Power approximation doesn't match precision")))))

  (define gen:list-of-flags
    (gen:let
     ([a gen:boolean] [b gen:boolean] [c gen:boolean] [d gen:boolean] [e gen:boolean] [f gen:boolean])
     (list a b c d e f)))

  (define-property
   match-number-of-y
   ([flags gen:list-of-flags])
   (let ([trues (count identity flags)])
     (check-equal? (length (next-y (context (make-sums '(1 2) '(1 4)) '(1 2) '(1 4) flags 0 0 0) 0))
                   trues)))

  (check-property match-number-of-y))

(module+ main
  (print-header initial-global-context)
  (let*-values ([(more? get) (sequence-generate table)])

    ;;; accums
    ;;; looped-structure
    (for/fold ([ctx initial-global-context] [n 0] #:result (void))
              ([x0 (in-generator (let loop ([x (- (context-start initial-global-context)
                                                  (* 2 (context-step initial-global-context)))])
                                   (begin
                                     (yield x)
                                     (loop (+ x (context-step initial-global-context))))))]
               #:break (>= n (context-count initial-global-context)))

      (match-let ([(list N SX SXX SXXX SXXXX SLnX SLnX2 SY SLnY SXLnY SLnXY SXY SLnXLnY SXXY)
                   (context-sums ctx)])

        (when (> N 1)
          ;;; body
          (pretty-print x0)

          (for ([y (next-y ctx x0)])
            (pretty-print #:sep ";" y))
          (newline))

        (values
         (if (more?)
             (let* ([xy (get)] [x (car xy)] [y (cdr xy)])

               (context (list (add1 N)
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
                              (+ SXXY (* x x y)))
                        (append (context-xs ctx) (list x))
                        (append (context-ys ctx) (list y))
                        (context-flags ctx)
                        (context-start ctx)
                        (context-step ctx)
                        (context-count ctx)))
             (context (list N SX SXX SXXX SXXXX SLnX SLnX2 SY SLnY SXLnY SLnXY SXY SLnXLnY SXXY)
                      (context-xs ctx)
                      (context-ys ctx)
                      (context-flags ctx)
                      (context-start ctx)
                      (context-step ctx)
                      (context-count ctx)))
         (add1 n))))))
