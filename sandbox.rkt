#lang racket
(define moja (list 1 2 3 4))
(define moja1 '(5 6 7 8))

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define schema (list (column-info 'city    'string)
		(column-info 'country 'string)
		 (column-info 'area    'number)
		 (column-info 'capital 'boolean)))


(define rows (list (list "Wrocław" "Poland"  293 #f)
				 (list "Warsaw"  "Poland"  517 #t)
				 (list "Poznań"  "Poland"  262 #f)
				 (list "Berlin"  "Germany" 892 #t)
				 (list "Munich"  "Germany" 310 #f)
				 (list "Paris"   "France"  105 #t)
				 (list "Rennes"  "France"   50 #f)))


(sort  rows (lambda (x y)(string<? (car x)(car y))))

{define (find-index col schema)
    (cond [(equal? col (column-info-name (car schema))) 0]
          [else
            (+ 1 (find-index col (rest schema)))
          ]
    )
}


; {define (in-1-and-2 tab1-schema tab2-schema)
; 		(define (helper tab1-schema tab2-schema acc)
; 			(cond [(null? tab1-schema) acc]
; 				  [else
; 					(if (equal? (column-info-name (car tab1-schema)) (column-info-name (car tab2-schema)))
; 						(helper (rest tab1-schema) tab2-schema (append (list (column-info-name (car tab1-schema))) acc))
; 						(helper (rest tab1-schema) tab2-schema acc)
; 					)
; 				  ]
; 			)
; 		)
; 	}


(string->symbol (string-append (symbol->string 'city) "2"))

'(1 2 3 4)


