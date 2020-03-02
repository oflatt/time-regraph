#lang racket

(require plot)
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
                 data))))

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

  ; hash of lists of data
  (define all-averages-data (make-hash))
  (for ([(key bench-data) data-table])
    (define table-file (open-output-file (build-path (current-directory) dir-name
                                                     (string-append key ".txt"))
                                         #:exists 'replace))
    (for ([row (hash-map bench-data
                         (lambda (num data)
                           (list num (compute-averages data)))
                         #t)])
      (unless (hash-has-key? all-averages-data (first row))
        (hash-set! all-averages-data (first row) (list)))
      (hash-set! all-averages-data
                 (first row)
                 (cons (second row) (hash-ref all-averages-data (first row))))
      (display-line (second row) table-file)))
  
  (define all-averages (make-hash))
  (for ([(key data) all-averages-data])
    (hash-set! all-averages key (compute-averages data)))
  (displayln name)
  (for ([row (hash-map all-averages (lambda (num data) (list num data)) #t)])
    (displayln row))

  data-table)


(define (get-total-time nodemap)
  (define data (hash-ref nodemap 5000))
  (for/list ([r data])
    (first r)))

(define (get-search-time nodemap)
  (define data (hash-ref nodemap 5000))
  (for/list ([r data])
    (fourth r)))


(define (make-search-plot utable rtable filename label get-function)
  ;; keys are benchmark and values are a pair of search time points
  (define vector-table (make-hash))
  (for ([(key nodemap) utable])
    (hash-set! vector-table key
               (map vector (get-function nodemap)
                    (get-function (hash-ref rtable key)))))

  (define all-points
    (flatten
     (for/list ([(key vectors) vector-table])
       vectors)))

  
  (plot-file
   (list (function (λ (x) x) #:color 0 #:style 'dot)
         (points all-points))
   (string-append filename ".png")
   #:x-label (string-append "upwards merging " label)
   #:y-label (string-append "rebuliding " label)))

(module+ main
  (define upwards-table (start-process "upwards"))
  (define rebuild-table (start-process "rebuilding"))
  (make-search-plot upwards-table rebuild-table "total-time" "total time" get-total-time)
  (make-search-plot upwards-table rebuild-table "search-time" "search time" get-search-time)
  )
