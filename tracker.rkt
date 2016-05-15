#lang racket

(require net/head)
(require net/http-client)
(require net/url)
(require openssl/sha1)

(require "utils.rkt")
(require "encoding.rkt")

(provide request-peers)

(define (request-peers tracker-url sha1-bstr my-peer-id [number-of-peers 10])
  (let* ([tracker-response (announce tracker-url
                                     sha1-bstr
                                     my-peer-id
                                     number-of-peers)]
         [peers (cadr (assoc #"peers"
                             (parse-dictionary (open-input-bytes
                                                tracker-response))))])
    (parse-peers peers)))

(define (parse-peers peers)
  (define (parse-peer peer)
    (let ([host (subbytes peer 0 4)]
          [port (subbytes peer 4 6)])
      (string-append "tcp://"
                     (string-join (map number->string
                                       (bytes->list host))
                                  ".")
                     ":"
                     (number->string (bytes->number port)))))
  (for/list ([i (range (/ (bytes-length peers) 6))])
    (parse-peer (subbytes peers
                          (* 6 i)
                          (+ (* 6 i) 6)))))

(define (announce base-url info-hash peer-id number-of-peers)
  ;; Queries the tracker at base-url for a list of peers.
  ;; TODO: Needs error handling
  (define (get-path base-url)
    (string-join
     (filter non-empty-string?
             (map (λ (x) (path/param-path x))
                  (url-path base-url)))
     "&"))

  (define query
    (bytestring-alist->form-urlencoded
     (list (cons 'info_hash info-hash)
           (cons 'peer_id peer-id)
           (cons 'port "6969")
           (cons 'uploaded "0")
           (cons 'downloaded "0")
           (cons 'left "0")
           (cons 'compact "1")
           (cons 'no_peer_id "1")
           (cons 'event "started")
           (cons 'ip "127.0.0.1")
           (cons 'numwant (number->string number-of-peers)))))

  (define-values (status headers in)
    (http-sendrecv (url-host base-url)
                   (string-append "/" (get-path base-url) "?" query)
                   #:port (or (url-port base-url) 80)
                   #:ssl? (equal? (url-scheme base-url) "https")
                   #:method "GET"))

  (let ([response-length
         (string->number (bytes->string/utf-8
                          (cdr (assoc #"Content-Length"
                                      (extract-all-fields
                                       (bytes-join headers #"\r\n"))))))])
    (read-bytes response-length in)))

;; tests
(module+ test
  (require rackunit)
  (check-equal? (parse-peers (bytes-append #"\n\1\1\1\2\232"
                                           #"\254\20\1\n\4\322"
                                           #"\300\250\0017\20\341"))
                '("tcp://10.1.1.1:666"
                  "tcp://172.16.1.10:1234"
                  "tcp://192.168.1.55:4321")))
