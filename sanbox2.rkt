#lang racket

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define schema (list (column-info 'city    'string)
		(column-info 'country 'string)
		 (column-info 'area    'number)
		 (column-info 'capital 'boolean)))

(define r1 (list "Warsaw"  "Poland"  517 #t))
(define r2  (list "Poznań"  "Poland"  262 #f))
(define r3 (list "Berlin"  "Germany" 892 #t))
(define r4 (list "Munich"  "Germany" 310 #f))


; przyjąłem, że #f < #t
(define (p<? l r) ; funkcja przekazująca prawidłowy predykat dla danego typu danych w wierszu
        (cond [(number? l) (< l r)]
              [(string? l) (string<? l r)]
              [(boolean? l) (cond [(equal? l r) #f]
                                  [(and (equal? l #f) (equal? r #t)) #t]
                                  [else #f])]
              ))

(define (p>? l r) ; funkcja przekazująca prawidłowy predykat dla danego typu danych w wierszu
        (cond [(number? l) (> l r)]
              [(string? l) (string>? l r)]
              [(boolean? l) (cond [(equal? l r) #f]
                                  [(and (equal? l #t) (equal? r #f)) #t]
                                  [else #f])]
              ))

(define (min-of-two row_1 row_2 cols row-schema)
    (define (helper row1 row2 cols row-schema acc1 acc2)
        (cond [(null? cols) acc1] 
              [(equal? (car cols) (column-info-name (car row-schema)))
                
                (cond [(p<? (car row1) (car row2)) acc1]
                      [(p>? (car row1) (car row2)) acc2]
                      [else 
                        (display "else \n") ; nie wchodzi w ogóle w tego else'a
                        (helper row_1 row_2 (rest cols) row-schema acc1 acc2)
                      ]
                )
              ]
              [else 
                (helper (rest row1) (rest row2) (rest cols) (rest row-schema) acc1 acc2) ; powinno tu być cols zamiast (rest cols), ale to wyrzuca contract violation
              ]
        )
    )
    (helper row_1 row_2 cols row-schema row_1 row_2)
)

; (define r3 (list "Berlin"  "Germany" 892 #t))
; (define r4 (list "Munich"  "Germany" 310 #f))

(min-of-two r4 r3 '(city country) schema)
(min-of-two r4 r3 '(country city) schema)
; powinno zwrócić 2 razy Berlin

(p>?  "Berlin" "Berlin")
