#lang racket

(require math/array)
(require math/matrix)

(define (read-csv-to-strings)
  (for/list ([xy (in-lines (current-input-port))])
    (string-split xy ";" #:trim? #t #:repeat? 2)))


(define (csv-function-tabulate csv)
  (map (lambda (xy) (cons (string->number (first xy)) (string->number (second xy)))) csv))


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
     [a (first coefficients)]
     [b (second coefficients)])
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
     [A (first coefficients)]
     [B (second coefficients)]
     [a (exp B)]
     [b A])
    (lambda (x) (* a (exp (* b x))))))


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
