#lang racket

(require "../regraph/main.rkt")
(require "../regraph/egraph.rkt")
(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)


(define rebuilding? (make-parameter #f))

(define rules-exprs-port (open-input-file "./rules.txt"))
(define rules-in (read rules-exprs-port))
(define rules-out (read rules-exprs-port))

(define end #f)

(define (read-math port)
  (let ([i (read port)])
    (cond
      [(equal? eof i)
       (set! end #t)
       empty]
      [(equal? i "NEW BATCH")
       empty]
      [else
       (cons i (read-math port))])))

;; spawns next regraph or returns false if its the end of file
(define (spawn-regraph port)
  (define exprs (read-math port))
  (if (empty? exprs)
      (if end
          #f
          (make-regraph (read-math port)))
      (make-regraph exprs)))

(define (spawn-all-regraphs port)
  (set! end #f)
  (define re (spawn-regraph port))
    (if re
        (cons re (spawn-all-regraphs port))
        empty))

(define (run-regraph regraph times node-limit iters-file)
  (define last-i
    (for/or ([i (range times)])
      (define initial-cnt (regraph-count regraph))
      ((rule-phase rules-in rules-out) regraph)
      (if
       (< initial-cnt (regraph-count regraph) node-limit)
       #f
       i)))
  (unless (rebuilding?)
    (displayln (+ last-i 1) iters-file)))

(define (render-regraph-info-with-port all-regraphs port data)
  (display (exact->inexact (/ (first data) (length all-regraphs))) port)
  (for ([ele (rest data)])
    (display ", " port)
    (display (exact->inexact (/ ele (length all-regraphs))) port))
  (display "\n" port))
  

(define (render-regraph-info all-regraphs time-file data)
  (render-regraph-info-with-port all-regraphs time-file data)
  (render-regraph-info-with-port all-regraphs (current-output-port) data))

(define (folder-string)
  (cond
    [(rebuilding?)
     "timing-rebuilding"]
    [else
     "timing-upwards"]))

(define (time-suite filename)
  (define average-port
    (open-input-file (build-path (current-directory)
                                 (folder-string)
                                 "averages.txt")
                     #:exists? 'replace))
  
  (for ([i (range 8)])
    (define exprs-name
      (substring (path->string filename) 0 (- (string-length (path->string filename)) 4)))
    (define node-limit (+ 2500 (* i 2500)))
    (display "Timing with node limit: ")
    (displayln (number->string node-limit))
    (define suite-port (open-input-file (build-path "exprs" filename)))
    (define time-file (open-output-file
                       (build-path (current-directory)
                                   (folder-string)
                                   (string-append (number->string node-limit) "-"
                                                  exprs-name "-total.txt"))
                       #:exists 'replace))
    (define iters-file-out
      (if (rebuilding?)
          #f
          (open-output-file
           (build-path (current-directory)
                       (string-append (number->string node-limit) "-"
                                      exprs-name "-iters.txt"))
           #:exists 'replace)))
      
    (define all-regraphs
      (spawn-all-regraphs suite-port))
    (define iters-file-in
      (if (rebuilding?)
          (open-input-file
           (build-path (current-directory)
                       (string-append (number->string node-limit) "-"
                                      exprs-name "-iters.txt")))
          #f))

    (define all-data
      (for/list ([regraph all-regraphs] [i (length all-regraphs)])
        (display "Regraph " )
        (displayln (number->string i))
        (flush-output)
        (define iteration-limit
          (if (rebuilding?)
              (read iters-file-in)
              10000000))
        (define begin-time (current-inexact-milliseconds))
        (define begin-merge merge-time)
        (define begin-rebuild rebuild-time)
        (run-regraph regraph iteration-limit node-limit iters-file-out)
        (define after (current-inexact-milliseconds))
        (define data
          (list
           (- after begin-time)
           (- merge-time begin-merge)
           (- rebuild-time begin-rebuild)))
        (render-regraph-info (list regraph) time-file
                             data)))
    (define averages
      (list
       (/ (sum
           (for/list ([a all-data]) (first a))) (length all-data))
       (/ (sum
           (for/list ([a all-data]) (second a))) (length all-data))
       (/ (sum
           (for/list ([a all-data]) (third a))) (length all-data))))
    (render-regraph-with-port
     (list regraph)
     average-port
     averages)))
     

(module+ main
  (command-line 
   #:program "time-rebuilding"
   #:once-each
   [("-r" "--rebuild") "Time regraph with rebuilding enabled"
                       (rebuilding? #t)])
  

  (for ([expr-file (directory-list (build-path (current-directory) "exprs"))])
    (displayln "#########################")
    (display "Timing file: ")
    (displayln (path->string expr-file))
    (time-suite expr-file)))
