#lang racket

(require "../regraph/main.rkt")
(require "../regraph/egraph.rkt")
(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)


(define rebuilding? (make-parameter #f))
(define number-timing-iterations (make-parameter 3))

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
(define (spawn-regraph port node-limit)
  (define exprs (read-math port))
  (if (empty? exprs)
      (if end
          #f
          (make-regraph (read-math port) #:limit node-limit))
      (make-regraph exprs #:limit node-limit)))


(define (spawn-all-regraphs port node-limit)
  (set! end #f)
  (define re (spawn-regraph port node-limit))
    (if re
        (cons re (spawn-all-regraphs port node-limit))
        empty))

(define (run-regraph regraph match-limit iters-file)
  (define last-i
    (for/or ([i (range 100000000000)])
      (define initial-cnt (regraph-count regraph))
      ((rule-phase rules-in rules-out #:match-limit match-limit) regraph)
      (if
       (and
        (< initial-cnt (regraph-count regraph))
        (or (not (regraph-limit regraph)) (< (regraph-count regraph) (regraph-limit regraph)))
        (or (not match-limit) (<= (regraph-match-count regraph) match-limit)))
       #f
       i)))
  (display "Last iteration: ")
  (println last-i)
  (unless (rebuilding?)
    (displayln (+ (regraph-match-count regraph) 1) iters-file)))

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
    (open-output-file (build-path (current-directory)
                                 (folder-string)
                                 "averages.txt")
                     #:exists 'replace))
  
  (for ([i (range (number-timing-iterations))])
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

    (define used-node-limit
      (if (rebuilding?)
          #f
          node-limit))
      
    (define all-regraphs
      (spawn-all-regraphs suite-port used-node-limit))
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
        (define match-limit
          (if (rebuilding?)
              (read iters-file-in)
              #f))
        (define begin-time (current-inexact-milliseconds))
        (define begin-merge merge-time)
        (define begin-rebuild rebuild-time)
        (define begin-find-matches find-matches-time)
        (run-regraph regraph match-limit iters-file-out)
        (define after (current-inexact-milliseconds))
        (define data
          (list
           (- after begin-time)
           (- merge-time begin-merge)
           (- rebuild-time begin-rebuild)
           (- find-matches-time begin-find-matches)))
        (render-regraph-info (list regraph) time-file
                             data)
        data))
    
    (define averages
      (list
       (foldr (lambda (d r) (+ (first d) r)) 0 all-data)
       (foldr (lambda (d r) (+ (second d) r)) 0 all-data)
       (foldr (lambda (d r) (+ (third d) r)) 0 all-data)
       (foldr (lambda (d r) (+ (fourth d) r)) 0 all-data)))
    
    (render-regraph-info-with-port
     all-regraphs
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
