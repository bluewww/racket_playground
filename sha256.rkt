#lang typed/racket
(require "binfile.rkt")
(require "modarith.rkt")
(require typed/rackunit)

(provide sha256 hash8->hex)

;; sha256 initial hash values
(: hsh-init (Vectorof Integer))
(define hsh-init
  (vector
   #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

;; sha256 round constants
(: ks (Vectorof Integer))
(define ks
  (vector
   #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
   #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
   #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
   #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
   #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
   #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

;; append original message,  one '1' bit, K '0' bits, length of msg encoded as 64 bit
;; big endian int
;; K is chosen such that L + 1 + K + 64 mod 512 = 0, K >= 0, K minimal
;; consider changing to bytestring since we need all that ugly casting to assure that we
;; have bytes
;; use (inst Vector Byte) (compiletime)
;; instead of run time casting
(: sha256-padding (-> Bytes Bytes))
(define (sha256-padding message)
  (let* ([L (* (bytes-length message) 8)]
         [K (modulo (- (+ L 1 64)) 512)])
    (bytes-append message
                   (bytes 128) ;since we add 1 bit anyways we always have to extend to 8 bit
                   (make-bytes (quotient (- K 7) 8) 0)
                   ;length encoded as 64 bit big endian integer
                   (integer->integer-bytes L 8 #f #t))))

;; process 512 bit chunk message
;; message is the new chunk to hash
;; hsh is the hash value of the previous chunks
(: sha256-step (-> (Vectorof Integer) Bytes (Vectorof Integer)))
(define (sha256-step hsh message)
  ;; group the vector of bytes message (512 bit in total)
  ;; into 512 bit vector of 32 bit integers
  (define message512
    (for/vector : (Vectorof Integer) ([i : Nonnegative-Integer (in-range 0 16)])
      (integer-bytes->integer message #f #t (+ 0 (* i 4)) (+ 4 (* i 4)))))
  ;; extend the message of 16 32bit integers to 64 32bit integers
  (define ws
    (for/fold ([acc : (Vectorof Integer) message512])
              ([i : Positive-Index (in-range 16 64)])
      (define w15 (vector-ref acc (- i 15)))
      (define w2  (vector-ref acc (- i 2)))
      (define w16 (vector-ref acc (- i 16)))
      (define w7  (vector-ref acc (- i 7)))
      (define s0 (xor32 (rotr32 w15 7) (xor32 (rotr32 w15 18) (shiftr32 w15 3))))
      (define s1 (xor32 (rotr32 w2 17) (xor32 (rotr32 w2 19) (shiftr32 w2 10))))
      (vector-append
       acc
       (vector (add32 w16 (add32 s0 (add32 w7 s1)))))))
  ;; one round of the sha256 compression step
  (: compress (-> Integer Integer (Vectorof Integer) (Vectorof Integer)))
  (define (compress k w v)
    (match v
      [(vector a b c d e f g h)
       (define s1 (xor32 (rotr32 e 6) (xor32 (rotr32 e 11) (rotr32 e 25))))
       (define ch (xor32 (and32 e f) (and32 (not32 e) g)))
       (define t1 (add32 h (add32 s1 (add32 ch (add32 k w)))))
       (define s0 (xor32  (rotr32 a 2) (xor32  (rotr32 a 13) (rotr32 a 22))))
       (define maj (xor32 (and32 a b) (xor32 (and32 a c) (and32 b c))))
       (define t2 (add32 s0 maj))
       (vector (add32 t1 t2) a b c (add32 d t1) e f g)]))
  ;; compose the list consisting of compress function (for each ks ws there is
  ;; function, 64 in total)
  (define dhsh
    (for/fold ([acc : (Vectorof Integer) hsh])
              ([f   : (-> (Vectorof Integer) (Vectorof Integer))
                    (vector-map (λ ([k : Integer] [w : Integer])
                                  (curry (curry compress k) w)) ks ws)])
      (f acc)))
  ;; add the new hash of this block to the previous hash value
  ;; this is the hash value up to this block
  (vector-map add32 dhsh hsh))
;; Note: curry doesnt seem to work with functions that take an arbitrary amount of
;; parameters, especially if it accepts also only one argument like '+'
;; workaround: force type: (curry (ann + (-> Number Number Number)) 2)

;; split up message into chunks of 64 bytes/512 bit and process block by block
(: sha256 (-> Bytes (Vectorof Integer)))
(define (sha256 message)
  (define padded-message (sha256-padding message))
  (define num-chunks (/ (bytes-length padded-message) 64))
  (for/fold : (Vectorof Integer)
      ([hsh-acc : (Vectorof Integer) hsh-init])
      ([i       : Nonnegative-Integer (in-range 0 num-chunks)])
    (sha256-step hsh-acc (subbytes padded-message (* i 64) (+ 64 (* i 64))))))

(: hash8->hex (-> (Vectorof Integer) String))
(define (hash8->hex hsh)
  (for/fold : String
      ([hacc : String ""])
      ([h : Integer hsh])
    (string-append hacc (~r h #:base 16 #:min-width 8 #:pad-string "0"))))


(module+ test
  (check-equal?
   (hash8->hex (sha256 #"abc"))
   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  (check-equal?
   (hash8->hex (sha256 #"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
   "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  (check-equal?
   (hash8->hex (sha256 (make-bytes 1000000 97)))
   "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"))



