#lang racket

(require regraph)

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

(define (render-regraph-info all-regraphs time-file data order)
  (display (exact->inexact (/(profile-total-time data) (length all-regraphs))) time-file)
  (display " " time-file)
  (display (for/or ([node (profile-nodes data)])
           (cond
             [(equal? (node-id node) 'merge-egraph-nodes!)
              (exact->inexact (/ (node-total node) (length all-regraphs)))]
             [else #f]))
         time-file)
  (displayln (for/or ([node (profile-nodes data)])
             (cond
               [(equal? (node-id node) 'egraph-rebuild)
                (exact->inexact (/ (node-total node) (length all-regraphs)))]
               [else #f]))
           time-file))

(define (folder-string)
  (cond
    [(rebuilding?)
     "timing-upwards"]
    [else
     "timing-rebuilding"]))

(define (time-suite filename)
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

    (for ([regraph all-regraphs] [i (length all-regraphs)])
      (display "Regraph " )
      (displayln (number->string i))
      (flush-output)
      (define iteration-limit
        (if (rebuilding?)
            (read iters-file-in)
            10000000))
      (profile-thunk
       (lambda () (run-regraph regraph iteration-limit node-limit iters-file-out))
       #:render (curry render-regraph-info (list regraph) time-file)))))
     

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
