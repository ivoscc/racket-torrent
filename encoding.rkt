#lang racket

(provide parse-torrent-file
         parse-dictionary
         bencode-dictionary)

(define (parse-torrent-file file-path)
  (call-with-input-file file-path
    (λ (in)
      (parse-dictionary in))))

(define (parse-dictionary input)
  (read-char input) ;; drop the #\d
  (define output (for/list ([i (in-naturals)]
                            #:break (eq? (next-type? input) 'end))
                   (list (parse-string input) (parse-next input))))
  (read-char input) ;; drop the trailing #\e
  output)

(define (bencode-dictionary input-hash)
  (bytes-append #"d"
                (apply bytes-append
                       (map (λ (item)
                              (bytes-append
                               (bencode-string (car item))
                               (bencode-item (cadr item))))
                            input-hash))
                #"e"))

;; utils
(define (read-string-until-char input stop-char)
  (let loop ([result '()])
    (let ([next-char (read-char input)])
      (cond [(or (eof-object? next-char) (char=? next-char stop-char))
             (list->string (reverse result))]
            [else (loop (cons next-char result))]))))

(define (next-type? input)
  (let ([next-char (peek-char input)])
    (cond [(or (eof-object? next-char) (char=? next-char #\e)) 'end]
          [(char=? next-char #\i) 'integer]
          [(char=? next-char #\l) 'list]
          [(char=? next-char #\d) 'dictionary]
          [(char-numeric? next-char) 'string])))

(define (parse-next input)
  (let ([next-item (next-type? input)])
    (cond [(eq? next-item 'string) (parse-string input)]
          [(eq? next-item 'integer) (parse-integer input)]
          [(eq? next-item 'list) (parse-list input)]
          [(eq? next-item 'dictionary) (parse-dictionary input)]
          [else null])))

(define (bencode-item item)
  (cond [(number? item) (bencode-integer item)]
        [(or (string? item) (bytes? item)) (bencode-string item)]
        [(dict? item) (bencode-dictionary item)]
        [(list? item) (bencode-list item)]))

;; parsers
(define (parse-string input)
  (let ([str-len (string->number (read-string-until-char input #\:))])
    (read-bytes str-len input)))

(define (valid-integer? str)
  (let* ([negative (equal? (substring str 0 1) "-")]
         [digits (if negative (substring str 1) str)])
    ;; check failure conditions
    (not (or
          ;; contains non-numeric characters
          (> (length (filter
                      (lambda (c) (not (char-numeric? c)))
                      (string->list digits)))
             0)
          ;; contains digit starting with 0 (other than "i0e")
          (and
           ;; first digit is 0
           (equal? "0" (substring digits 0 1))
           (or
            ;; ... and there're more digits afterwards
            (> (string-length digits) 1)
            ;; ... or the whole number is negative
            negative))))))

(define (parse-integer input)
  (read-char input) ;; drop the #\i
  (let ([candidate (read-string-until-char input #\e)])
    (if (valid-integer? candidate)
        (string->number candidate)
        (error 'parse-integer "Failed to convert non-integer: ~a" candidate))))

(define (parse-list input)
  (read-char input) ;; drop the #\l
  (define output (for/list ([i (in-naturals)]
                            #:break (eq? (next-type? input) 'end))
                   (parse-next input)))
  (read-char input) ;; drop the trailing #\e
  output)

;; encoders
(define (bencode-string input)
  (let ([byte-string (cond [(bytes? input) input]
                           [(string? input) (string->bytes/utf-8 input)])])
    (bytes-append
     (string->bytes/utf-8 (number->string (bytes-length byte-string)))
     #":"
     byte-string)))

(define (bencode-integer num)
  (string->bytes/utf-8
   (string-append "i"
                  (number->string num)
                  "e")))

(define (bencode-list input-list)
  (let loop ([result '(#"l")]
             [remaining-items input-list])
    (cond [(empty? remaining-items)
           (apply bytes-append (reverse (cons #"e" result)))]
          [else
           (loop (cons (bencode-item (first remaining-items)) result)
                 (rest remaining-items))])))

(module+ test
  (require rackunit)

  ;; Test read-string-until-char
  (let ([input-string "Hi there"])
    (check-equal? (read-string-until-char
                   (open-input-string input-string) #\space)
                  "Hi")
    (check-equal? (read-string-until-char
                   (open-input-string input-string) #\a)
                  "Hi there"))

  ;; Test parse-string
  (check-equal? (parse-string (open-input-string "3:abc")) #"abc")
  (check-equal? (parse-string (open-input-string "2:abc")) #"ab")
  (check-equal? (parse-string (open-input-string "0:abc")) #"")

  ;; Test parse-integer
  (check-equal? (parse-integer (open-input-string "i123e")) 123)
  (check-equal? (parse-integer (open-input-string "i0e")) 0)
  (check-equal? (parse-integer (open-input-string "i-10e")) -10)
  (check-exn exn:fail?
             (lambda () (parse-integer (open-input-string "i1.2e")) ))
  (check-exn exn:fail?
             (lambda () (parse-integer (open-input-string "i03e")) ))
  (check-exn exn:fail?
             (lambda () (parse-integer (open-input-string "i-0e")) ))

  ;; Test parse-list
  (check-equal? (parse-list (open-input-string "l4:spam4:eggsi42ee"))
                '(#"spam" #"eggs" 42))
  (check-equal? (parse-list (open-input-string "li10el6:nestedi-1ee"))
                '(10 (#"nested" -1)))

  ;; Test parse-dictionary
  (check-equal? (parse-dictionary
                 (open-input-string "d3:cow3:moo4:spam4:eggse"))
                '((#"cow" #"moo") (#"spam" #"eggs")))
  (check-equal? (parse-dictionary (open-input-string "d4:spaml1:a1:bee"))
                '((#"spam" (#"a" #"b"))))
  (check-equal? (parse-dictionary (open-input-string "de"))
                '())

  ;; Test bencode-string
  (check-equal? (bencode-string "") #"0:")
  (check-equal? (bencode-string #"") #"0:")
  (check-equal? (bencode-string #"hello") #"5:hello")
  (check-equal? (bencode-string #"áéíóú") #"5:\341\351\355\363\372")

  ;; ;; Test bencode-integer
  (check-equal? (bencode-integer 0) #"i0e")
  (check-equal? (bencode-integer -4) #"i-4e")
  (check-equal? (bencode-integer 10) #"i10e")

  ;; ;; Test bencode-list
  (check-equal? (bencode-list '(1 2 3)) #"li1ei2ei3ee")
  (check-equal? (bencode-list '(1 "wat" 3)) #"li1e3:wati3ee")
  (check-equal? (bencode-list '("nested" (1 2 "three")))
                #"l6:nestedli1ei2e5:threeee")

  ;; ;; Test bencode-dictionary
  (check-equal? (bencode-dictionary '(("key" "val")))
                #"d3:key3:vale")
  (check-equal? (bencode-dictionary '(("key" "val") ("key2" "val2")))
                #"d3:key3:val4:key24:val2e")
  (check-equal? (bencode-dictionary '(("key" ("nested" "wat"))))
                #"d3:keyl6:nested3:watee")
  (check-equal? (bencode-dictionary '(("key" (("nested" "wat")))))
                #"d3:keyd6:nested3:watee"))
