#lang racket

(provide (struct-out column-info)
				 (struct-out table)
				 (struct-out and-f)
				 (struct-out or-f)
				 (struct-out not-f)
				 (struct-out eq-f)
				 (struct-out eq2-f)
				 (struct-out lt-f)
				 table-insert
				 table-project
				 table-sort
				 table-select
				 table-rename
				 table-cross-join
				 table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
	(table
	 (list (column-info 'city    'string)
				 (column-info 'country 'string)
				 (column-info 'area    'number)
				 (column-info 'capital 'boolean))
	 (list (list "Wrocław" "Poland"  293 #f)
				 (list "Warsaw"  "Poland"  517 #t)
				 (list "Poznań"  "Poland"  262 #f)
				 (list "Berlin"  "Germany" 892 #t)
				 (list "Munich"  "Germany" 310 #f)
				 (list "Paris"   "France"  105 #t)
				 (list "Rennes"  "France"   50 #f))))

(define countries
	(table
	 (list (column-info 'country 'string)
				 (column-info 'population 'number))
	 (list (list "Poland" 38)
				 (list "Germany" 83)
				 (list "France" 67)
				 (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

; Wstawianie

; sprawdzenie poprawnośći;
;   - czy liczba kolumn się zgadza
;   - czy elementy są właściwych typów
;   - w przypadku niezgodności zakańczamy procedure błędem
;     (wywołanie error)
;   - kolejność wierszy po wstawieniu nieistotna
{define (list-len lista) ; funkcja sprawdzająca zgodność ilości pól
		(cond [(null? lista) 0]
					[else
						(+ 1 (list-len (rest lista)))])}

{define (check-type str) ; funkcja pomocnicza do check-accordance
		(cond [(equal? str 'boolean) (lambda (x) (boolean? x))]
					[(equal? str 'string) (lambda (x) (string? x))]
					[(equal? str 'number) (lambda (x) (number? x))])}

{define (check-accordance schema row) ; funkcja sprawdzająca zgodność typów pól
		(cond [(and (null? schema) (null? row)) #t] 
					[(and 
						 ((check-type (column-info-type (car schema))) (car row))
						 (check-accordance (rest schema) (rest row))
						) #t]
					[else #f])}

(define (table-insert row tab)
	(cond [(not (= (list-len row) (list-len (table-schema tab))))
			(error "Nie zgadza się liczba kolumn")
		  ]
          [(not (check-accordance (table-schema tab) row)) 
            (error "Nie zgadzają się typy pól")
          ]
          [else
            (table (table-schema tab) (cons row (table-rows tab)))
          ]
		)
	)

; Projekcja

{define (get-col col tab-schema) ; funkcja znajdująca daną kolumne
    (cond [(equal? col (column-info-name (car tab-schema)))
            (column-info col (column-info-type (car tab-schema)))]
          [else 
            (get-col col (rest tab-schema))])}

{define (get-schema cols tab) ; funkcja tworząca schemat kolumn tabeli
    (cond [(null? cols) null]
          [else
            (cons (get-col (car cols) (table-schema tab)) (get-schema (rest cols) tab))])}

{define (parse-row row n-schema o-schema)
    (define (helper row n-schema o-schema acc)
        (cond [(or (null? n-schema) (null? o-schema)) acc]
              [(equal? (column-info-name (car o-schema)) (column-info-name (car n-schema))) 
                (helper (rest row) (rest n-schema) (rest o-schema) (append acc (list (car row))))
              ]
              [else
                (helper (rest row) n-schema (rest o-schema) acc)
              ]
        
        )
    )
    (helper row n-schema o-schema (list))
}

(define (table-project cols tab)
	(define (helper rows acc)
		(cond [(null? rows) acc]
			  [else
			  	(helper (rest rows) (append acc (list (parse-row (car rows) (get-schema cols tab) (table-schema tab)))))
			  ]
		)
	)
	(table (get-schema cols tab) (helper (table-rows tab) (list))); uzupelnic o wiersze z wyjętymi danyi kolumnami - iterowac się jednoczesnie po początkowym schemacie i wierszach i w momencie, gdy natrafimy na dobrą kolumne to ją zwracać
)

; Sortowanie

(define (table-sort cols tab)
	null
	)

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab)
	null
	)

; Zmiana nazwy

{define (table-rename col ncol tab)
    (define schemat (table-schema tab))
    (define (helper col ncol schema)
        (cond [(equal? (column-info-name (car schema)) col)
                (cons (column-info ncol (column-info-type (car schema))) (rest schema))
              ]
              [else 
                (cons (car schema) (helper col ncol (rest schema)))
              ]
        )
    )
    (table (helper col ncol schemat) (table-rows tab))
    
}

; Złączenie kartezjańskie

{define (cross row table-rows) ; funkcja łącząca dany wiersz z każdym wierszem z table-rows
	(define (helper row table-rows acc)
		(cond [(null? table-rows) acc]
			  [else
				(helper row (cdr table-rows) (append acc (list (append row (car table-rows)))))
			  ]	  
		)
	)
	(helper row table-rows (list))
}

{define (table-cross-join tab1 tab2) ; funkcja właściwa
	(define rows-tab1 (table-rows tab1))
	(define rows-tab2 (table-rows tab2))
	(define (helper tabs1 tabs2 acc)
		(cond [(null? tabs1) acc]
			  [else 
				(helper (rest tabs1) tabs2 (append acc (cross (car tabs1) tabs2)))
			  ]
		)
	)
	(table (append (table-schema tab1) (table-schema tab2)) (helper rows-tab1 rows-tab2 (list)) )
}

; Złączenie

(define (table-natural-join tab1 tab2)
	null
	)

(define (print-table tab) ; Funkcja pomocnicza
  (define (print-col-names t)
    (cond[(null? t) (display "\n")]
         [else (display (column-info-name (car t)))
               (display ",  ")
               (print-col-names (cdr t))]))
  (define (print-rows t)
    (cond[(null? t) 
            (display "\n")]
         [else 
            (display (car t)) 
            (display "\n") 
            (print-rows (cdr t))]))
  (print-col-names (table-schema tab))
  (print-rows (table-rows tab))) ;table-rows daje mi listę z rekordami w tabeli