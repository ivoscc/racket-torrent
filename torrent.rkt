#lang racket

(require net/url)
(require openssl/sha1)

(require "encoding.rkt")

(define (get-piece-length t piece-index)
  (let (;; pieces are 0-indexed
        [piece-number (add1 piece-index)]
        [number-of-pieces (torrent-number-of-pieces t)]
        [regular-piece-length (torrent-piece-length t)])
    (cond [(< piece-number number-of-pieces) regular-piece-length]
          [(= piece-number number-of-pieces) (- (torrent-data-length t)
                                                (* (sub1 number-of-pieces)
                                                   regular-piece-length))]
          [else 0])))

(define (create-output-file-container name)
  (let ([path (build-path (current-directory)
                          (string-append name ".temp"))])
    (when (not (directory-exists? path))
      (display "DEBUG: Creating output directory.")
      (make-directory path))
    path))

(define (get-missing-pieces path number-of-pieces)
  (let ([downloaded-pieces (map (lambda (item)
                                  (string->number
                                   (path->string item)))
                                (directory-list path))])
    (for/list ([i (range number-of-pieces)]
               #:unless (member i downloaded-pieces))
      i)))


(struct torrent (name sha1 announce data-length piece-hashes
                      piece-length number-of-pieces
                      output-path
                      [missing-pieces #:mutable]))

(define (make-torrent torrent-file-path)
  (define (get-value key item [default null])
    (if (null? default)
        (cadr (assoc key item))
        (with-handlers ([exn:fail:contract?
                         (λ (e) default)])
          (cadr (assoc key item)))))
  (let* ([metainfo (parse-torrent-file torrent-file-path)]
         [info (get-value #"info" metainfo)]
         [name (bytes->string/utf-8 (get-value #"name" info))]
         [data-length (get-value #"length" info)]
         [pieces (get-value #"pieces" info)]
         [piece-length (get-value #"piece length" info)]
         [number-of-pieces (/ (bytes-length pieces) 20)]
         [output-path (create-output-file-container name)])

    (torrent name
             (sha1-bytes (open-input-bytes (bencode-dictionary info)))
             ;; TODO: this could be the list of trackers #"announce list"
             (string->url (bytes->string/utf-8 (get-value #"announce" metainfo)))
             data-length
             (for/list ([i (range number-of-pieces)])
               (subbytes pieces (* 20 i) (* 20 (add1 i))))
             piece-length
             number-of-pieces
             output-path
             (get-missing-pieces output-path number-of-pieces))))


(provide get-piece-length
         make-torrent
         torrent-announce
         torrent-data-length
         torrent-name
         torrent-number-of-pieces
         torrent-missing-pieces
         torrent-output-path
         torrent-piece-hashes
         torrent-piece-length
         torrent-sha1)
