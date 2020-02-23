#lang racket

(require regraph)

(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)


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
    (for/last ([i (range times)]
               #:break (and (regraph-limit regraph) (>= (regraph-count regraph) (regraph-limit regraph))))
      ((rule-phase rules-in rules-out) regraph)
      i))
  (unless rebuilding?
    (writeln iters-file (+ last-i 1))))

(define (render-regraph-info all-regraphs time-file data order)
  (writeln (exact->inexact (/(profile-total-time data) (length all-regraphs))) time-file)
  (for ([node (profile-nodes data)])
    (cond
      [(equal? (node-id node) 'merge-egraph-nodes!)
       (writeln (exact->inexact (/ (node-total node) (length all-regraphs))) merge-time-file)]
      [(equal? (node-id node) 'egraph-rebuild)
       (writeln (exact->inexact (/ (node-total node) (length all-regraphs))) rebuild-time-file)])))

(define rebuilding? (make-parameter #f))
(define (folder-string)
  (cond
    [rebuilding?
     "timing-upwards"]
    [else
     "timing-rebuilding"]))


(define (time-suite filename)
  (for ([i (range 8)])
    (define node-limit (+ 2500 (* i 2500)))
    (define suite-port (open-input-file (build-path "exprs" filename)))
    (define time-file (open-output-file
                       (build-path (current-directory)
                                   (folder-string)
                                   (string-append (number->string node-limit) " "
                                                  (path->string filename) "-total"))
                       #:exists 'replace))
    (define iters-file-out
      (if rebuilding?
          #f
          (open-output-file
           (build-path (current-directory)
                       (string-append (number->string node-limit) " "
                                      (path->string filename) "-iters"))
           #:exists 'replace)))
      
    (define all-regraphs
      (spawn-all-regraphs suite-port))
    (define iters-file-in
      (if rebuilding?
          (open-input-file
           (build-path (current-directory)
                       (string-append (number->string node-limit) " "
                                      (path->string filename) "-iters")))
          #f))

    (for ([regraph all-regraphs])
      (define iteration-limit
        (if rebuilding?
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
    (time-suite expr-file)))
