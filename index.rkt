#lang racket
(require (only-in xml write-xexpr))
(require racket/date)

(define (total-time row)
  (~r (second row) #:precision '(= 1)))
(define (congruence-time row)
  (~r (+ (third row) (fourth row)) #:precision '(= 1)))
(define (congruence-detail row)
  (format " (~a + ~a)" (~r (third row) #:precision '(= 1)) (~r (fourth row) #:precision '(= 1))))
(define (search-time row)
  (~r (fifth row) #:precision '(= 1)))

(define (data-table c1-name data-um data-rb)
  `(table
    (thead
     (tr (th ,c1-name) (th ([colspan "2"]) "Total (ms)") (th ([colspan "2"]) "Congruence (ms)") (th ([colspan "2"]) "Search (ms)"))
     (tr ([class "right"]) (th) (th "UM") (th "RB") (th "UM") (th "RB") (th "UM") (th "RB")))
    (tbody
     ,@(for/list ([um data-um] [rb data-rb])
         `(tr (th ,(~a (first um)))
              (td ,(total-time um)) (td ,(total-time rb))
              (td ,(congruence-time um))
              (td (span ([title ,(congruence-detail rb)]))
                  ,(congruence-time rb))
              (td ,(search-time um)) (td ,(search-time rb)))))))

(define (make-html port node-data-um node-data-rb benchmark-data-um benchmark-data-rb)
  (define um5k (dict-ref node-data-um "5000"))
  (define rb5k (dict-ref node-data-rb "5000"))

  (define congruence-speedup (/ (+ (second um5k) (third um5k)) (+ (second rb5k) (third rb5k))))
  (define searching-speedup (/ (fourth um5k) (fourth rb5k)))
  (define overall-speedup (/ (first um5k) (first rb5k)))

  (write-xexpr
   `(html
     (head
      (meta ([charset "utf-8"]))
      (link ([rel "stylesheet"] [href "index.css"]))
      (title "Regraph evaluation for " ,(date->string (current-date))))
     (body
      (h1  "Regraph evaluation for " ,(date->string (current-date)))
      (p "Rebuilding is " (strong ,(~r (* (- overall-speedup 1) 100) #:precision '(= 2)) "%") " faster,"
         " with " (strong ,(~r congruence-speedup #:precision '(= 2)) "×") " faster congruence closure"
         " and " (strong ,(~r (* (- searching-speedup 1) 100) #:precision '(= 2)) "%") " faster searching.")

      ,(data-table "Suite" benchmark-data-um benchmark-data-rb)
      
      (figure
       (img ([src "search-time.png"]))(img ([src "total-time.png"])))

      ,(data-table "Node Limit" node-data-um node-data-rb)

      ))
   port))

(define (read-file port)
  (for/list ([line (in-port read-line port)])
    (let ([line (string-split (string-trim line) ",")])
      (cons (first line) (map string->number (rest line))))))

(module+ main
  (command-line 
   #:args (table-um table-rb benchmark-table-um benchmark-table-rb output)
   (call-with-output-file
       output #:exists 'replace
       (λ (p)
         (make-html p
                    (call-with-input-file table-um read-file)
                    (call-with-input-file table-rb read-file)
                    (call-with-input-file benchmark-table-um read-file)
                    (call-with-input-file benchmark-table-rb read-file))))))
