#lang racket
(require (only-in xml write-xexpr))
(require racket/date)

(define (data-table c1-name data-um data-rb)
  `(table
    (thead
     (tr (th ,c1-name) (th "Variant") (th "Total (ms)") (th "Merge (ms)") (th "Rebuild (ms)") (th "Search (ms)")))
    (tbody
     ,@(apply
        append
        (for/list ([um data-um] [rb data-rb])
          (list
           `(tr (th ([rowspan "2"]) ,(~a (first um))) (th "Upward Merging") ,@(for/list ([cell (rest um)]) `(td ,(~r cell #:precision 1))))
           `(tr (th "Rebuilding") ,@(for/list ([cell (rest rb)]) `(td ,(~r cell #:precision 1))))))))))

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
      (title "Regraph evaluation results as of " ,(date->string (current-date))))
     (body
      (h1  "Regraph evaluation results as of " ,(date->string (current-date)))
      (p "Rebuilding is " (strong ,(~r (* (- overall-speedup 1) 100) #:precision '(= 2)) "%") " faster,"
         " with " (strong ,(~r congruence-speedup #:precision '(= 2)) "×") " faster congruence closure"
         " and " (strong ,(~r (* (- searching-speedup 1) 100) #:precision '(= 2)) "%") " faster searching.")
      ,(data-table "Node Limit" node-data-um node-data-rb)
      
      (figure
       (img ([src "search-time.png"]))(img ([src "total-time.png"])))

      ,(data-table "Benchmark Name" benchmark-data-um benchmark-data-rb)

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
