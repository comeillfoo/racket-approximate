#lang racket


(define (read-csv-to-strings)
  (for/list ([xy (in-lines (current-input-port))])
    (string-split xy ";" #:trim? #t #:repeat? 2)))


(define (csv-function-tabulate csv)
  (map (lambda (xy) (cons (string->number (first xy)) (string->number (second xy)))) csv))


(csv-function-tabulate (read-csv-to-strings))
