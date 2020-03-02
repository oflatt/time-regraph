#lang racket

(define folder-out (make-parameter "processed"))

(define (read-all-lines port)
  (define line (read-line port))
  (if
   (equal? line eof)
   (list)
   (cons line (read-all-lines port))))

(define (get-data data-file-path)
  (define data-port (open-input-file data-file-path))
  (define data
    (for/list ([line (read-all-lines data-port)])
      (for/list
          ([string-num (string-split (string-replace line " " "") ",")])
        (string->number string-num))))
  data)


(define (compute-averages all-data)
  (define sums
    (list
     (foldr (lambda (d r) (+ (first d) r)) 0 all-data)
     (foldr (lambda (d r) (+ (second d) r)) 0 all-data)
     (foldr (lambda (d r) (+ (third d) r)) 0 all-data)
     (foldr (lambda (d r) (+ (fourth d) r)) 0 all-data)))
  (for/list ([e sums])
    (/ e (length all-data))))

(define (process-data data filename table)
  (unless (equal? (length data) 0)
    (define elements (string-split (path->string filename) "-"))
    (unless (not (equal? (length elements) 4))
      (define num-size (string->number (first elements)))
      (define bench-name (second elements))
      (unless
          (hash-has-key?
           table bench-name)
        (hash-set! table bench-name (make-hash)))
      (hash-set! (hash-ref table bench-name)
                 num-size
                 (compute-averages data)))))

(define (display-line data port)
  (display (first data) port)
  (for ([ele (rest data)])
    (display ", " port)
    (display ele port))
  (display "\n" port))

(define (start-process name)
  (define data-table (make-hash))
  (for ([data-file (directory-list (build-path (current-directory)
                                               (string-append "timing-" name)))])
    (process-data
     (get-data (build-path (current-directory)
                           (string-append "timing-" name)
                           data-file))
     data-file
     data-table))

  (define dir-name (string-append "tables-" name))
  (unless (directory-exists? dir-name)
    (make-directory (build-path (current-directory) dir-name)))
  (for ([(key averages) data-table])
    (define table-file (open-output-file (build-path (current-directory) dir-name
                                                     (string-append key ".txt"))
                                         #:exists 'replace))
    (for ([row (hash-map averages
                         (lambda (num data)
                           (list num data))
                         #t)])
      (display-line (second row) table-file))))

(module+ main
  (start-process "upwards")
  (start-process "rebuilding")
  )
