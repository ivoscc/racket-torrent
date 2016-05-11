#lang racket

(require racket/match)

(require "torrent.rkt")
(require "utils.rkt")

(define (merge-pieces t)
  ;; Once all the pieces have been downloaded, merge the contents
  ;; of the temp folder into a single file.
  ;; TODO: consider multifile torrents?
  (define path (torrent-output-path t))
  (call-with-output-file (torrent-name t)
    (lambda (out)
      (for ([piece-index (range (torrent-number-of-pieces t))])
        (call-with-input-file (build-path path (number->string piece-index))
          (lambda (in)
            (copy-port in out))
          #:mode 'binary)))
    #:mode 'binary)
  (delete-directory/files path))

(define (launch-io-worker t)
  (thread
   (lambda ()
     (let loop []
       (define evt (sync (thread-receive-evt)))
       (match (thread-receive)
         [(list 'piece piece-index data)
          (let ([piece-valid (verify-sha1-hash
                              (list-ref (torrent-piece-hashes t) piece-index)
                              data)]
                [output-file (build-path (torrent-output-path t)
                                         (number->string piece-index))])
            (cond [piece-valid
                   (unless (file-exists? output-file)
                     (call-with-output-file output-file
                       (lambda (out)
                         (write-bytes data out))
                       #:mode 'binary)
                     (printf "IO: Successfully wrote piece ~s\n" piece-index)
                     )
                   ]
                  [else (displayln "IO: Discarding invalid piece.")]))
          (loop)]
         ['stop (displayln "IO: Stopping IO worker.")]
         [else (displayln "IO: Unknown command.")
               (loop)])))))

(provide launch-io-worker
         merge-pieces)
