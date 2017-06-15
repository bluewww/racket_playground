#lang typed/racket
(require "binfile.rkt")
(require "modarith.rkt")
 ;remove this garbage later
 (: testfunc (-> Input-Port (Listof Byte) (Listof Byte)))
 (define (testfunc p lst)
   (match (read-byte p)
     [(? byte? b)   (testfunc p (cons b lst))]
     [(? eof-object?) lst]))

 (: adding (-> Integer Integer Integer))
 (define (adding a b)
   (+ a b))

 (: subbing (-> Integer Integer Integer))
 (define (subbing a b)
   (- a b))

(define h0 #x6a09e667)
(define h1 #xbb67ae85)
(define h2 #x3c6ef372)
(define h3 #xa54ff53a)
(define h4 #x510e527f)
(define h5 #x9b05688c)
(define h6 #x1f83d9ab)
(define h7 #x5be0cd19)
