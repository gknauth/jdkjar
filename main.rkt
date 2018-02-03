#lang racket
;; jdkjar/main.rkt
;; Copyright Geoffrey S. Knauth. See file "info.rkt".

(require racket/set
         racket/string
         file/unzip
         file/sha1)

(define (jdkjar jarfile [verbose? true])
  (let ([ht (make-hash)]
        [fr (jar->listof-jdk-classfile jarfile)])
    (for-each (λ (x)
                (if (hash-has-key? ht (first x))
                    (let ([hs (hash-ref ht (first x))])
                      (set-add! hs (second x)))
                    (let ([hs (mutable-set)])
                      (set-add! hs (second x))
                      (hash-set! ht (first x) hs))))
              fr)
    (printf "~a" (make-human-readable jarfile ht verbose?))))

; string (hashof string (setof byte-string)) boolean -> string
(define (make-human-readable jarfile ht verbose?)
  (let* ([n (hash-count ht)])
    (string-append (~a "In: " jarfile "\n")
                   (cond [(= n 0) "no classes found\n"]
                         [(= n 1) (~a "Java " (hash-iterate-key ht (hash-iterate-first ht))
                                      " is the JRE bytecode target of all classes\n" )]
                         [(> n 1) (multi-java-ht->string ht verbose?)]
                         [else (error "strange hash-count value: ~a" n)]))))

; (hashof string (setof byte-string)) boolean -> string
(define (multi-java-ht->string ht verbose?)
  (define (helper ht verbose?)
    (map (λ (x)
           (let* ([k (first x)]
                  [hs (second x)]
                  [n (set-count hs)])
             (~a "Java " k " is the JRE bytecode target of " n " class" (if (= n 1) "" "es") "\n"
                 (if verbose?
                     (apply string-append
                            (for/list ([x (sort (set->list hs) bytes<?)])
                              (~a "  " x "\n")))
                     ""))))
         (sort
          (hash-map ht (λ (k v)
                         (list k v)))
          (λ (a b) (string<=? (first a) (first b))))))
  (apply string-append (helper ht verbose?)))

; string -> (listof (string string))
(define (jar->listof-jdk-classfile jarfile)
  (let* ([in (open-input-file jarfile #:mode 'binary)]
         [directory-entries (read-zip-directory in)]
         [classfile-entries (zipdir->classfiles directory-entries)])
    (map (λ (classfile-entry)
           (call-with-classfile-entry in directory-entries (bytes->string/utf-8 classfile-entry) examine-class-entry))
         classfile-entries)))

(define (zipdir->classfiles zipdir)
  (filter
   (λ (x)
     (let ([s (bytes->string/utf-8 x)])
       (and (string-suffix? s ".class") (not (string-contains? s "$")))))
   (zip-directory-entries zipdir)))

; Based on call-with-unzip-entry except we already have directory-entries
; and don't need to regenerate it repeatedly.
(define (call-with-classfile-entry jarfile directory-entries entry-file user-proc)
  (let ([temp-dir #f])
    (dynamic-wind
     (lambda ()
       (set! temp-dir (make-temporary-file "ziptmp~a" 'directory)))
     (lambda ()
       (unzip-entry jarfile
                    directory-entries 
                    (path->zip-path entry-file)
                    (make-filesystem-entry-reader #:dest temp-dir #:exists 'replace))
       (user-proc temp-dir entry-file))
     (lambda ()
       (delete-directory/files temp-dir)))))

(define (examine-class-entry temp-dir entry-file)
  (let ([classfile-path (build-path temp-dir entry-file)])
    (with-input-from-file classfile-path
      (λ ()
        (let* ([bytes (read-bytes 8 (current-input-port))]
               [first4bytes (subbytes bytes 0 4)]
               [major-version (bytes-ref bytes 7)]
               [is-classfile (equal? first4bytes #"\xca\xfe\xba\xbe")])
          (if is-classfile
              (list (java-version major-version) entry-file)
              null)))
      #:mode 'binary)))

(define (format-8-bytes bytes)
  (string-append (~a (bytes->hex-string bytes))))

(define (java-version major-version)
  (cond [(eq? major-version 46) "1.2"]
        [(eq? major-version 47) "1.3"]
        [(eq? major-version 48) "1.4"]
        [(eq? major-version 49) "5"]
        [(eq? major-version 50) "6"]
        [(eq? major-version 51) "7"]
        [(eq? major-version 52) "8"]
        [(eq? major-version 53) "9"]
        [else "unknown"]))

;; --inspiration from--
;; https://stackoverflow.com/questions/22972176/unzip-an-entry-to-memory-in-racket
;; https://github.com/racket/racket/blob/master/racket/collects/file/unzip.rkt


(module+ test
  ;; Tests to be run with raco test
  (require rackunit)
  (define j6-only
    (let ([ht (make-hash)]
          [hs (mutable-set)])
      (set-add! hs #"a6.class")
      (set-add! hs #"b6.class")
      (hash-set! ht "6" hs)
      ht))
  (define j6-j7
    (let ([ht (make-hash)]
          [hs6 (mutable-set)]
          [hs7 (mutable-set)])
      (set-add! hs6 #"a6.class")
      (set-add! hs6 #"b6.class")
      (hash-set! ht "6" hs6)
      (set-add! hs7 #"a7.class")
      (set-add! hs7 #"b7.class")
      (hash-set! ht "7" hs7)
      ht))
  (define j6-j7-j8
    (let ([ht (make-hash)]
          [hs6 (mutable-set)]
          [hs7 (mutable-set)]
          [hs8 (mutable-set)])
      (set-add! hs6 #"a6.class")
      (set-add! hs6 #"b6.class")
      (set-add! hs6 #"c6.class")
      (hash-set! ht "6" hs6)
      (set-add! hs7 #"a7.class")
      (set-add! hs7 #"b7.class")
      (hash-set! ht "7" hs7)
      (set-add! hs8 #"a8.class")
      (hash-set! ht "8" hs8)
      ht))
  (check-equal? (make-human-readable "foo6.jar" j6-only false)
                "In: foo6.jar\nJava 6 is the JRE bytecode target of all classes\n")
  (check-equal? (make-human-readable "foo6-7.jar" j6-j7 false)
                (string-append
                 "In: foo6-7.jar\n"
                 "Java 6 is the JRE bytecode target of 2 classes\n"
                 "Java 7 is the JRE bytecode target of 2 classes\n"))
  (check-equal? (make-human-readable "foo6-7-8.jar" j6-j7-j8 false)
                (string-append
                 "In: foo6-7-8.jar\n"
                 "Java 6 is the JRE bytecode target of 3 classes\n"
                 "Java 7 is the JRE bytecode target of 2 classes\n"
                 "Java 8 is the JRE bytecode target of 1 class\n"))
  (check-equal? (make-human-readable "foo6-7-8.jar" j6-j7-j8 false)
                (string-append
                 "In: foo6-7-8.jar\n"
                 "Java 6 is the JRE bytecode target of 3 classes\n"
                 "Java 7 is the JRE bytecode target of 2 classes\n"
                 "Java 8 is the JRE bytecode target of 1 class\n"))
  (check-equal? (make-human-readable "foo6-7-8.jar" j6-j7-j8 true)
                (string-append
                 "In: foo6-7-8.jar\n"
                 "Java 6 is the JRE bytecode target of 3 classes\n  a6.class\n  b6.class\n  c6.class\n"
                 "Java 7 is the JRE bytecode target of 2 classes\n  a7.class\n  b7.class\n"
                 "Java 8 is the JRE bytecode target of 1 class\n  a8.class\n"))
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
