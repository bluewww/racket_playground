
(module modarith typed/racket

  (provide add32 mul32 rotr32 xor32 and32 not32 shiftr32 testi)
  ;;consider operator overloading?

  (: mod32 (-> Integer Integer))
  (define (mod32 m) (bitwise-and m #xffffffff))

  ;;TODO: generalize to fold?
  (: add32 (-> Integer Integer Integer))
  (define (add32 n m)
    (mod32 (+ n m)))

  (: mul32 (-> Integer Integer Integer))
  (define (mul32 n m)
    (mod32 (* n m)))

  (: rotr32 (-> Integer Integer Integer))
  (define (rotr32 n m)
    ;; TODO: range check m with types? contracts do this nicer
    (if (not (and (< 0 m) (< m 32)))
        (error "shift values of of range")
        (bitwise-ior
         (bitwise-and (arithmetic-shift n (- m)) (arithmetic-shift #xffffffff (- m)))
         (bitwise-and
          (arithmetic-shift n (- 32 m))
          (bitwise-and (bitwise-not (arithmetic-shift #xffffffff (- m))) #xffffffff)))))

  ;; TODO: enforce n to be positive
  (: shiftr32 (-> Integer Integer Integer))
  (define (shiftr32 n m)
    (bitwise-and (arithmetic-shift n (- m)) (arithmetic-shift #xffffffff (- m))))

  (: xor32 (-> Integer Integer Integer))
  (define (xor32 n m)
    (bitwise-and (bitwise-xor n m) #xffffffff))

  (: and32 (-> Integer Integer Integer))
  (define (and32 n m)
    (bitwise-and (bitwise-and n m) #xffffffff))

  (: not32 (-> Integer Integer))
  (define (not32 n)
    (bitwise-and (bitwise-not n) #xffffffff))

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
