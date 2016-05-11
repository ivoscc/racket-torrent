#lang racket

(require net/head)
(require net/http-client)
(require net/uri-codec)
(require net/url)
(require openssl/sha1)

(require "encoding.rkt")
(require "io.rkt")
(require "peers.rkt")
(require "torrent.rkt")
(require "tracker.rkt")
(require "utils.rkt")

(define (get-next-available-peer peer-list piece-index number-of-pieces)
  (with-handlers ([exn:fail:contract?
                   (lambda (exn) (void))])
    (first (takef peer-list
                  (lambda (peer-worker-pair)
                    (and
                     ;; peer is ready
                     (peer-ready (car peer-worker-pair))
                     ;; peer is not choking us
                     (not (peer-peer-choking (car peer-worker-pair)))
                     ;; peer has the piece we want
                     (peer-has-piece? (car peer-worker-pair)
                                      piece-index
                                      number-of-pieces)))))))

(define (stop-workers peer-workers)
  (for-each
   (lambda (peer-worker-pair)
     (let ([w (cadr peer-worker-pair)])
       (when (thread-send w 'stop #f)
         (thread-wait w))))
   (remove-dead-workers peer-workers)))

(define (remove-dead-workers peer-workers)
  (filter (lambda (x)
            (let ([p (car x)]
                  [w (cadr x)])
              (not (or (void? w)
                       (and (thread? w) (thread-dead? w))))))
          peer-workers))

(define (peer-has-piece? p piece-index number-of-pieces)
  (bitwise-bit-set?
   (peer-pieces p)
   (- (sub1 number-of-pieces) piece-index)))

(define (download-remaining-pieces t
                                   missing-pieces
                                   peer-workers)
  (cond
      [(empty? peer-workers)
       (displayln "All workers are dead.")]
      [(empty? missing-pieces)
       (displayln "Finished requesting all pieces.")]
      [else
       (let ([available-peer (get-next-available-peer peer-workers
                                                      (first missing-pieces)
                                                      (torrent-number-of-pieces t))])
         (cond
           [(void? available-peer)
            (sleep 1)
            (download-remaining-pieces t
                                       missing-pieces
                                       (remove-dead-workers peer-workers))]
           [(not (peer-am-interested (car available-peer)))
            (set-peer-am-interested! (car available-peer) #t)
            (thread-send (cadr available-peer) 'interested #f)
            (download-remaining-pieces t
                                       missing-pieces
                                       (remove-dead-workers peer-workers))]
           [else
            (let ([p (car available-peer)]
                  [w (cadr available-peer)])
              (thread-send w (list 'request (first missing-pieces)) #f)
              (set-peer-ready! p #f))
            (download-remaining-pieces t
                                       (rest missing-pieces)
                                       (remove-dead-workers peer-workers))]))]))

;; Example
;;
;; Parse the torrent file
(define t (make-torrent "../torrent/sample/ubuntu.torrent"))
(define my-peer-id (generate-peer-id))
;; Request some peers
(define peer-urls (request-peers (torrent-announce t)
                                 (torrent-sha1 t)
                                 my-peer-id))
;; Create peer structs
(define peer-list (map (lambda (url)
                         (make-peer url (torrent-piece-length t)))
                       peer-urls))
;; Launch the io thread that'll write the pieces to the temporary directory
(define io-worker (launch-io-worker t))

;; Launch a worker thread for each peer
(define peer-workers
  (for/list ([p peer-list])
    (list p (create-peer-worker p t io-worker my-peer-id))))

;; Start asking the peer workers to download pieces
(download-remaining-pieces t
                           (torrent-missing-pieces t)
                           peer-workers)

;; Hopefully there's no more missing pieces and we can rebuild the file
(when (= 0 (length (torrent-missing-pieces t)))
  (merge-pieces t))

;; Stop any peer worker thread that's still alive
(stop-workers peer-workers)
