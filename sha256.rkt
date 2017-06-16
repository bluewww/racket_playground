#lang typed/racket
(require "binfile.rkt")
(require "modarith.rkt")

;change scope of constants to not make it bleed into other code
;sha256 initial hash values
(define h0 #x6a09e667)
(define h1 #xbb67ae85)
(define h2 #x3c6ef372)
(define h3 #xa54ff53a)
(define h4 #x510e527f)
(define h5 #x9b05688c)
(define h6 #x1f83d9ab)
(define h7 #x5be0cd19)

;sha256 round constants
(: k (Vectorof Integer))
(define k
  (vector
   #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
   #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
   #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
   #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
   #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
   #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

;append original message,  one '1' bit, K '0' bits, length of msg encoded as 64 bit
;big endian int
;K is chosen such that L + 1 + K + 64 mod 512 = 0, K >= 0, K minimal
;consider changing to bytestring since we need all that ugly casting to assure that we
;have bytes
(: sha256-padding (-> (Vectorof Byte) (Vectorof Byte)))
(define (sha256-padding message)
  (let* ([L (* (vector-length message) 8)]
         [K (modulo (- (+ L 1 64)) 512)])
    (vector-append message
                   (cast (vector 128) (Vectorof Byte)) ;since we add 1 bit anyways we always have to extend to 8 bit
                   (cast (make-vector (quotient (- K 7) 8) 0) (Vectorof Byte))
                   ;lenght encoded as 64 bit big endian integer
                   (cast
                    (list->vector (bytes->list (integer->integer-bytes L 8 #f #t)))
                    (Vectorof Byte)))))

;process 512 bit chunk
;(define (process-chunk c))
