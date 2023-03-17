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
{define (list-len lista)
    (cond [(null? lista) 0]
          [else
            (+ 1 (list-len (rest lista)))
          ]
    )
}

(define (table-insert row tab)
  (cond [(not (= (list-len row) (list-len (table-schema tab))))
          (error "Nie zgadza się liczba kolumn")
        ]
    )
  )

; Projekcja

(define (table-project cols tab)
  null
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

(define (table-rename col ncol tab)
  null
  )

; Złączenie kartezjańskie

(define (table-cross-join tab1 tab2)
  null
  )

; Złączenie

(define (table-natural-join tab1 tab2)
  null
  )




{define (get-col col tab-schema)
    (cond [(equal? col (column-info-name (car tab-schema)))
            (column-info col (column-info-type (car tab-schema)))]
          [else 
            (get-col col (rest tab-schema))])}

{define (get-schema cols tab)
    (cond [(null? cols) null]
          [else
            (cons (get-col (car cols) (table-schema tab)) (get-schema (rest cols) tab))
          ]
    )
}

; '(capital area)       ; dlaczego nie ma różnicy między tymi dwoma zakomentowanymi ?
; (list 'capital 'area)
(get-schema '(capital area) cities)