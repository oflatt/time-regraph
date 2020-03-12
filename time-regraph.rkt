#lang racket

(require "./regraph/main.rkt")
(require "./regraph/egraph.rkt")
(require (except-in profile profile))
(require profile/render-text)
(require profile/analyzer)


(define rebuilding? (make-parameter #f))
(define iteration-options '(2500 5000 7500))

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

(define (run-regraph regraph match-limit limits-file match-count-port)
  (define last-i
    (for/or ([i (range 100000000000)])
      (define initial-cnt (regraph-count regraph))
      ((rule-phase rules-in rules-out #:match-limit match-limit) regraph)
      (fprintf match-count-port "~a\n" (regraph-match-count regraph))
      (if
       (and
        (< initial-cnt (regraph-count regraph))
        (or (not (regraph-limit regraph)) (< (regraph-count regraph) (regraph-limit regraph)))
        (or (not match-limit) (<= (regraph-match-count regraph) match-limit)))
       #f
       i)))
  (printf "Last iteration: ~a\n" last-i)
  (fprintf limits-file "~a\n" (+ (regraph-match-count regraph) 1)))

(define (render-regraph-info-with-port all-regraphs port data)
  (fprintf port "~a\n"
           (string-join (map (λ (ele) (~a (exact->inexact (/ ele (length all-regraphs))))) data) ",")))

(define (render-regraph-info all-regraphs time-file data)
  (render-regraph-info-with-port all-regraphs time-file data)
  (render-regraph-info-with-port all-regraphs (current-output-port) data)
  (flush-output))

(define (time-suite filename folder)
  (define average-port
    (open-output-file (build-path (current-directory) folder "averages.txt")
                      #:exists 'replace))
  (define match-count-port
    (open-output-file (build-path (current-directory) folder "match-counts-verification.txt") #:exists 'replace))
  
  (for ([node-limit (in-list iteration-options)])
    (define-values (suite-folder suite-file unused-flag) (split-path (string->path filename)))
    (define exprs-name (path->string (path-replace-extension suite-file "")))
    (define suite-port (open-input-file filename))
    (printf "Timing with node limit: ~a\n" (number->string node-limit))
    (define time-file (open-output-file
                       (build-path (current-directory)
                                   folder
                                   (string-append (number->string node-limit) "-"
                                                  exprs-name "-total.txt"))
                       #:exists 'replace))
    (define limits-file-name
      (build-path (current-directory) (format "~a-~a-match-limits.txt" node-limit exprs-name)))

    (define limits-file-out
      (if (rebuilding?)
          (open-output-nowhere)
          (open-output-file limits-file-name #:exists 'replace)))

    (define all-regraphs
      (spawn-all-regraphs suite-port (and (not (rebuilding?)) node-limit)))

    (define limits-file
      (if (rebuilding?)
          (open-input-file limits-file-name)
          #f))

    (define all-data
      (for/list ([regraph all-regraphs] [i (length all-regraphs)])
        (fprintf (current-output-port)
                 "Regraph ~a\n" i)
        (flush-output)

        (define match-limit
          (if (rebuilding?)
              (read limits-file)
              #f))

        (define begin-time (current-inexact-milliseconds))
        (define begin-merge merge-time)
        (define begin-rebuild rebuild-time)
        (define begin-find-matches find-matches-time)
        (run-regraph regraph match-limit limits-file-out match-count-port)
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
    (rebuilding? #t)]
   #:args (folder . expr-files)
   (for ([expr-file expr-files])
     (printf "#########################\nTiming file: ~a\n" expr-file)
     (time-suite expr-file folder))))
