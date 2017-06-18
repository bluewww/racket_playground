
(module modarith typed/racket
  (provide add32 mul32 rotr32 xor32 and32 not32 testi)
  ;;consider operator overloading?
  ;;later change that to & using a mask
  (define pow32 (expt 2 32))

  ;;TODO: generalize to fold?
  (: add32 (-> Integer Integer Integer))
  (define (add32 n m)
    (modulo (+ n m) pow32))

  (: mul32 (-> Integer Integer Integer))
  (define (mul32 n m)
    (modulo (* n m) pow32))

  (: rotr32 (-> Integer Integer Integer))
  (define (rotr32 n m)
    ;; TODO: how do you make asserts work? seems to require a weird type
    ;; see (:type number?) => (-> Any Boolean : Number)
    ;;(assert m (λ (x : Real) (and (< 0 x) (< x 32))))
    (if (not (and (< 0 m) (< m 32)))
        (error "shift values of of range")
        (bitwise-ior
         (bitwise-and (arithmetic-shift n (- m)) (arithmetic-shift #xffffffff (- m)))
         (bitwise-and
          (arithmetic-shift n (- 32 m))
          (bitwise-and (bitwise-not (arithmetic-shift #xffffffff (- m))) #xffffffff)))))

  (: xor32 (-> Integer Integer Integer))
  (define (xor32 n m)
    (bitwise-and (bitwise-xor n m)) #xffffffff)

  (: and32 (-> Integer Integer Integer))
  (define (and32 n m)
    (bitwise-and (bitwise-and n m)) #xffffffff)

  (: not32 (-> Integer Integer))
  (define (not32 n)
    (bitwise-and (bitwise-not n)) #xffffffff)

  (: testi (-> Real Real))
  (define (testi a)
    (assert a real?)
    a))




;#lang racket
;(require racket/require
;         ;; grab all "mod*" names, but get them without the "mod", so
;         ;; `+' and `expt' is actually `mod+' and `modexpt'
;         (filtered-in (λ(n) (and (regexp-match? #rx"^mod" n)
;                                 (regexp-replace #rx"^mod" n "")))
;                      math)
;         (only-in math with-modulus))
;(define (f x) (+ (expt x 100) x 1))
;(with-modulus 13 (f 10))
;;; => 1
