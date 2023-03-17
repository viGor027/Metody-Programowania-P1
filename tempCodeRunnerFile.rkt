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