#lang racket

(require net/url)
(require openssl/sha1)
(require racket/match)
(require racket/tcp)

(require "io.rkt")
(require "torrent.rkt")
(require "utils.rkt")

(define PROTOCOL-HEADER (bytes-append (bytes 19) #"BitTorrent protocol"))
(define RESERVED-BYTES #"\0\0\0\0\0\0\0\0")
(define HANDSHAKE-HEADER (bytes-append PROTOCOL-HEADER RESERVED-BYTES))

(define BLOCK-SIZE (expt 2 14))

;; Message types
(define TYPE-CHOKE 0)
(define TYPE-UNCHOKE 1)
(define TYPE-INTERESTED 2)
(define TYPE-NOT-INTERESTED 3)
(define TYPE-HAVE 4)
(define TYPE-BITFIELD 5)
(define TYPE-PIECE 7)

;; Messages
(define INTERESTED-PAYLOAD #"\0\0\0\1\2")


(struct peer ([id #:mutable]
              url
              [ready #:mutable]
              [current-piece-index #:mutable]
              [current-blocks #:mutable]
              ;; pieces bitmap stored as a integer
              [pieces #:mutable]
              ;; am_choking: this client is choking the peer
              [am-choking #:mutable]
              ;; am_interested: this client is interested in the peer
              [am-interested #:mutable]
              ;; peer_choking: peer is choking this client
              [peer-choking #:mutable]
              ;; peer_interested: peer is interested in this client
              [peer-interested #:mutable])
  #:transparent)

(define (make-peer address piece-length)
  ;; Starting status
  (peer null
        (string->url address)
        ;; ready
        #f
        ;; current-piece-index
        null
        ;; current blocks
        (make-vector (/ piece-length BLOCK-SIZE) #"")
        ;; pieces
        0
        ;; am_choking
        #t
        ;; am_interested
        #f
        ;; peer_choking
        #t
        ;; peer_interested
        #f))

(define (request-payload piece-index block-offset block-length)
  (bytes-append #"\0\0\0\r\6"
                (bytes-pad (number->bytes piece-index) 4)
                (bytes-pad (number->bytes block-offset) 4)
                (bytes-pad (number->bytes block-length) 4)))

(define (request-piece target-peer piece-index piece-length tcp-out)
  (let*-values ([(full-blocks remainder) (quotient/remainder piece-length
                                                             BLOCK-SIZE)]
                [(blocks) (append (build-list full-blocks
                                              (lambda (x)
                                                (list x BLOCK-SIZE)))
                                  (if (> remainder 0)
                                      (list (list full-blocks remainder))
                                      null))])
    (for-each
     (lambda (block)
       (let* ([block-offset (car block)]
              [current-block-size (cadr block)]
              [payload (request-payload piece-index
                                        (* block-offset BLOCK-SIZE)
                                        current-block-size)])
         (write-bytes payload tcp-out)
         (flush-output tcp-out)))
     blocks)))

(define (parse-piece payload)
  (let ([piece-index (bytes->number (subbytes payload 0 4))]
        [block-offset (bytes->number (subbytes payload 4 8))]
        [data (subbytes payload 8)])
    (values piece-index block-offset data)))

(define (update-current-blocks! peer block-offset data)
  (let ([current-blocks (peer-current-blocks peer)])
    (vector-set! current-blocks (/ block-offset BLOCK-SIZE) data)))

(define (clear-current-piece! peer)
  (set-peer-current-blocks! peer
                            (make-vector
                             (vector-length (peer-current-blocks peer))
                             #"")))

(define (flush-complete-piece! peer io-worker)
  (let ([current-blocks (peer-current-blocks peer)])
    (thread-send io-worker
                 (list 'piece
                       (peer-current-piece-index peer)
                       (apply bytes-append (vector->list
                                            current-blocks))))
    (clear-current-piece! peer)))

(define (piece-complete? target-torrent target-peer piece-index)
  (>= (foldl + 0 (map bytes-length
                      (vector->list (peer-current-blocks target-peer))))
      (get-piece-length target-torrent piece-index)))


(define (peer-worker target-torrent target-peer tcp-in tcp-out io-worker)

  (define (command-handler evt)
    ;; Handle control commands coming through the mailbox
    (match (thread-receive)
      ['stop (displayln "Shutting down worker.")
             (custodian-shutdown-all (current-custodian))]
      ['interested (write-bytes INTERESTED-PAYLOAD tcp-out)
                   (flush-output tcp-out)
                   (displayln "Telling peer we're interested.")]
      [(list 'request piece-index)
       (unless (equal? piece-index (peer-current-piece-index target-peer))
         (clear-current-piece! target-peer)
         (set-peer-current-piece-index! target-peer piece-index))
       (request-piece target-peer
                      piece-index
                      (get-piece-length target-torrent piece-index)
                      tcp-out)]
      [else (displayln "Unknown command.")]))

  (define (response-handler incoming-bytes)
    ;; handle incoming messages from the peer
    (let* ([message-length (bytes->number incoming-bytes)]
           [message (read-bytes message-length tcp-in)]
           [type (when (> message-length 0)
                   (bytes->number (subbytes message 0 1)))]
           [payload (when (> message-length 0)
                      (subbytes message 1))])
      (cond
        [(void? type) (displayln "Got keep alive.")]
        [(= type TYPE-CHOKE)
         (displayln "Peer has choked us.")
         (set-peer-peer-choking! target-peer #t)]
        [(= type TYPE-UNCHOKE)
         (displayln "Peer has unchoked us.")
         (set-peer-peer-choking! target-peer #f)]
        [(= type TYPE-INTERESTED)
         (displayln "Peer is interested.")
         (set-peer-peer-interested! target-peer #t)]
        [(= type TYPE-NOT-INTERESTED)
         (displayln "Peer is not interested.")
         (set-peer-peer-interested! target-peer #f)]
        [(= type TYPE-HAVE)
         (define piece-number (bytes->number payload))
         (set-peer-pieces! target-peer (bitwise-ior
                            (peer-pieces target-peer)
                            (arithmetic-shift 1 piece-number)))]
        [(= type TYPE-BITFIELD)
         (set-peer-pieces! target-peer (bytes->number payload))]
        [(= type TYPE-PIECE)
         (let-values ([(piece-index block-offset data) (parse-piece payload)])
           (update-current-blocks! target-peer block-offset data)
           (when (piece-complete? target-torrent target-peer piece-index)
             (flush-complete-piece! target-peer io-worker)
             (set-peer-ready! target-peer #t)))]
        [else
         (displayln "Method not implemented.")])))

  (printf "Launching worker for peer: '~a'\n" (peer-id target-peer))
  (let loop []
    (sync (handle-evt (thread-receive-evt)
                      ;; Respond to C&C commands
                      command-handler)
          (handle-evt (read-bytes-evt 4 tcp-in)
                      ;; Respond to incoming messages from the peer
                      (lambda (incoming-bytes)
                        (when (eof-object? incoming-bytes)
                          (displayln "Got eof.")
                          (custodian-shutdown-all (current-custodian)))
                        (response-handler incoming-bytes))))
    (loop)))

(define (handshake-send/receive tcp-in tcp-out torrent-sha my-peer-id
                                #:timeout [timeout 10])
  (define (parse-handshake-response response)
    (with-handlers ([exn:fail:contract?
                     (lambda (exn)
                       (displayln "Bad handshake response."))])
      (when (and (equal? (subbytes response 0 20) PROTOCOL-HEADER)
                 (equal? (subbytes response 28 48) torrent-sha))
        (subbytes response 48))))
  ;; Send handshake
  (write-bytes (bytes-append HANDSHAKE-HEADER
                             torrent-sha
                             (string->bytes/utf-8 my-peer-id))
               tcp-out)
  (flush-output tcp-out)
  ;; Wait for a valid handshake response
  (let ([response (sync/timeout timeout (read-bytes-evt 68 tcp-in))])
    (if (or (not response) (eof-object? response))
        (displayln "Peer didn't respond to handshake.")
        (parse-handshake-response response))))


(define (create-peer-worker target-peer target-torrent io-worker my-peer-id)
  ;; Perform handshake with peer. If successful, launch and return
  ;; a thread worker to use for communicating with the peer.
  (parameterize ([current-custodian (make-custodian)])
    (with-handlers ([exn:fail:network?
                     (lambda (exn)
                       (displayln "Failed to connect to peer.")
                       (custodian-shutdown-all (current-custodian)))])
      (define-values (tcp-in tcp-out)
        (tcp-connect (url-host (peer-url target-peer))
                     (url-port (peer-url target-peer))))

      (let ([peer-id (handshake-send/receive tcp-in tcp-out
                                             (torrent-sha1 target-torrent)
                                             my-peer-id
                                             #:timeout 5)])
        (cond [(void? peer-id)
               (displayln "Failed to perform handshake.")
               (custodian-shutdown-all (current-custodian))]
              [else
               (set-peer-id! target-peer peer-id)
               (set-peer-ready! target-peer #t)
               (thread
                (lambda ()
                  (with-handlers
                    ([exn:fail:network?
                      (lambda (exn)
                        (displayln "Peer connection close unexpectedly."))])
                    (peer-worker target-torrent
                                 target-peer
                                 tcp-in
                                 tcp-out
                                 io-worker))))])))))
(provide make-peer
         create-peer-worker
         peer-ready
         peer-peer-choking
         peer-pieces
         peer-current-blocks
         peer-am-interested
         set-peer-am-interested!
         set-peer-ready!)
