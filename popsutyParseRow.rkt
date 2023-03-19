#lang racket

; (define wiersz (list "Munich"  "Germany" 310 #f))

; {define (parse-row row n-schema o-schema)
;     (define (helper row n-schema o-schema acc)
;         (define nowy (column-info-name (car n-schema)))
;         (define stary (column-info-name (car o-schema)))
;         (cond [(or (null? n-schema) (null? o-schema)) acc]
;               [(equal? stary nowy) 
;                 (helper (rest row) (rest n-schema) (rest o-schema) (append acc (list (car row))))
;               ]
;               [else
;                 (helper (rest row) n-schema (rest o-schema) acc)
;               ]
        
;         )
;     )
;     (helper row n-schema o-schema (list))
; }
; (parse-row wiersz (get-schema '(city area) cities) (table-schema cities))