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
  ;zamienia podany element na stringa
  ;rozpotrujemu prawdę i fałsz w kolejności leksykograficznej tak jak stringi
  (define (el->str el)
    (cond[(boolean? el) (if el "T" "F")] 
         [(number? el) (number->string el)]
         [(symbol? el) (symbol->string el)]
         [else el]
	)
  )
  
  ;bierze kolumne wiersza i zwraca ją w stringu
  (define (col-of-row->string cols exact-col row)
    (cond[(equal? (column-info-name (car cols)) exact-col)
            (el->str (car row))]
         [else
            (col-of-row->string (cdr cols) exact-col (cdr row))]))

  ;sklei wszystkie podane kolumny wiersza do strigna
  (define (glue row cols-to-glue acc)
    (cond[(null? cols-to-glue) acc]
         [else
          (glue row (cdr cols-to-glue) (string-append acc (col-of-row->string (table-schema tab) (car cols-to-glue) row)))
		 ]
	))

  ;składa poprzednie funkcje, żeby w sorcie było czytelnie
  (define (tool element)
    (glue element cols ""))

  (table
    (table-schema tab)
    (sort (table-rows tab) #:key tool string<?)))

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab)
		; znajduje odpowiednią kulumna danego wiersza i zwraca jej zawartość
		{define (get-col-name row col schema)
			(cond [(null? schema) (error "nie znaleziono kolumny")] 
				  [(equal? col (column-info-name (car schema))) (car row)]
				  [else (get-col-name (rest row) col (rest schema))]
			)
		}
		; sprawdza czy dany wiersz spełnia warunek
		{define (form-to-p? form row)
			(cond [(and-f? form) 
					(and (form-to-p? (and-f-l form) row) (form-to-p? (and-f-r form) row))
				  ]
				  [(or-f? form) 
				  	(or (form-to-p? (and-f-l form) row) (form-to-p? (and-f-r form) row))
				  ]
				  [(not-f? form) 
				  	(not (form-to-p? (not-f-e form) row))
				  ]
				  [(eq-f? form) 
				  	(equal? (eq-f-val form) (get-col-name row (eq-f-name form) (table-schema tab)))
				  ]
				  [(eq2-f? form) 
				  	(equal? 
						(get-col-name row (eq2-f-name form) (table-schema tab))
						(get-col-name row (eq2-f-name2 form) (table-schema tab))
					)
				  ]
				  [(lt-f? form) 
				  	(<						
						(get-col-name row (lt-f-name form) (table-schema tab))
						(lt-f-val form)
					)
				  ]
				  [else form]
			)
		}

		;przechodzi przez wszystkie wiersze i zwraca liste tych wierszy, które spełniają formułe
		{define (get-rows formula rows)
			(define (helper formula rows acc)
				(cond [(null? rows) acc]
					  [else
					  	(if (form-to-p? formula (car rows))
							(helper formula (rest rows) (append (list (car rows)) acc))
							(helper formula (rest rows) acc)
						)
					  ]
				)
			)
			(helper formula rows '())
		}

	(table (table-schema tab) (get-rows form (table-rows tab)))
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
	;;;PIERWSZA PAUZA
	;zamiena kolumny tabeli w liste
	(define (columns->list tab)
		(define (helper cols acc)
			(cond [(null? cols) acc]
				  [else
				  	(helper (rest cols) (append (list (column-info-name (car cols))) acc))
				  ]
			)
		)
		(helper (table-schema tab) empty)
	)
	; funkcja znajdująca część wpsólną kolumn pierwszej i drugiej tabeli
	(define (get-repeats t1 t2)
		(set-intersect (columns->list t1) (columns->list t2))
	)

	;funkcja zamieniająca nazwy column w tabeli2, które są również w tabeli1(!!!zwraca zmienioną tabele2!!!)
	(define (rename-repetitions reps)
		(define (helper reps acc)
			(cond [(null? reps) acc]
				  [else
				  	(helper (rest reps)
					 (table-rename (car reps) (string->symbol (string-append (symbol->string (car reps)) "2")) acc) )
				  ]
			)
		)
	(helper reps tab2)
	)
	;;;KONIEC PIERWSZEJ PAUZY

	;;;DRUGA/TRZECIA PAUZA
	(define cross-with-reps (table-cross-join tab1 (rename-repetitions (get-repeats tab1 tab2)))) ; tabela z drugiej pauzy
	
	(define (get-equal-rows-by-col reps) ; trzecia pauza
		(define (helper reps acc)
			(cond [(null? reps) acc]
				  [else 
				  	(helper 
						(rest reps)
						(append acc (table-rows (table-select (eq2-f (car reps) (string->symbol (string-append (symbol->string (car reps)) "2"))) cross-with-reps)))
					)
				  ]
			)
		)
		(helper reps empty)
	)
	
	(define (no-renamed-cols-list) ; tworzy liste kolumn, ale bez kolumn, których nazwa została zmieniona
		(define cols-with-reps (reverse (columns->list cross-with-reps)))
		(define repeated-cols (get-repeats tab1 tab2))
		(define (helper rep-cols cols-w-reps)
			(cond [(null? rep-cols) cols-w-reps]
				  [else
				  	(helper (rest rep-cols) (remove (string->symbol (string-append (symbol->string (car rep-cols)) "2")) cols-w-reps))
				  ]
			)
		)
		(helper repeated-cols cols-with-reps)
	)
	(define 3cia (table (table-schema cross-with-reps) (get-equal-rows-by-col (get-repeats tab1 tab2)))) ; tabela z trzeciej pauzy
	(table-project (no-renamed-cols-list) 3cia)
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
  (print-rows (table-rows tab)))


(print-table (table-natural-join cities countries))