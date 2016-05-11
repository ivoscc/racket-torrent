#lang racket

(require net/uri-codec)
(require openssl/sha1)

(define RESERVED-OR-UNSAFE
  (map (λ (c) (char->integer c))
       (string->list (string-append
                      ;; reserved
                      ";/?:@=&$-_.+!*'(),"
                      ;; unsafe
                      "<>\"#%{}|\\^~[]`"))))

(define (url-encodable? code-point)
  (and (> code-point 32)
       (< code-point 127)
       (not (member code-point RESERVED-OR-UNSAFE))))

;; racket's uri-encode works with strings only :(
(define (uri-encode-bytestring bytestring)
  (apply string-append
         (for/list ([i (bytes->list bytestring)])
           (cond
             [(url-encodable? i) (string (integer->char i))]
             [else (string-append "%" (string-upcase (number->string i 16)))]))))

(define (bytestring-alist->form-urlencoded parameters)

  (define (uri-encode-string-or-bytestring input)
    (cond [(string? input) (uri-encode input)]
          [(bytes? input) (uri-encode-bytestring input)]))

  (string-join
   (map (λ (value-pair)
          (string-join
           (list (symbol->string (car value-pair))
                 (uri-encode-string-or-bytestring (cdr value-pair)))
           "="))
        parameters)
   "&"))

(define (bytes->number bstr)
  (string->number (bytes->hex-string bstr) 16))

(define (number->bytes num)
  (define str (number->string num 16))
  (hex-string->bytes (if (= 0 (modulo (string-length str) 2))
                         str
                         (string-append "0" str))))

(define (bytes-pad bstr len)
  (define bstr-len (bytes-length bstr))
  (if (<= bstr-len len)
      (bytes-append (make-bytes (- len bstr-len ) 0) bstr)
      bstr))

(define (verify-sha1-hash sha1 bstr)
  (equal? sha1 (sha1-bytes (open-input-bytes bstr))))

(define (random-string strlen [charset "0123456789"])
  (define options-length (string-length charset))
  (list->string (for/list ([i (range strlen)])
                  (string-ref charset (random options-length)))))

(define (generate-peer-id)
  (string-append "-ZZ0001-" (random-string 12)))

(provide bytes-pad
         bytestring-alist->form-urlencoded
         generate-peer-id
         number->bytes
         random-string
         uri-encode-bytestring
         verify-sha1-hash
         bytes->number)

;; tests
(module+ test
  (require rackunit)

  (check-equal?
   (uri-encode-bytestring (hex-string->bytes "123456789abcdef123456789abcdef123456789a"))
   "%124Vx%9A%BC%DE%F1%23Eg%89%AB%CD%EF%124Vx%9A")

  (check-equal? (bytes-pad #"hi" 1) #"hi")
  (check-equal? (bytes-pad #"hi" 5) #"\0\0\0hi")

  (check-equal? (number->bytes 0) #"\0")
  (check-equal? (number->bytes 64) #"@")
  (check-equal? (number->bytes 666) #"\2\232")

  (check-equal? (bytes->number #"\0") 0)
  (check-equal? (bytes->number #"@") 64)
  (check-equal? (bytes->number #"\2\232") 666))
