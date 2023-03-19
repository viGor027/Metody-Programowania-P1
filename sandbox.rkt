#lang racket
(define moja (list 1 2 3 4))
(define moja1 '(5 6 7 8))

(append (append '() (list(car moja)))  (list (car(rest moja))))

(list)