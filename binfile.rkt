; Reminder: either use #lang typed/racket or (module ... typed/racket)
; if you use both then the (module ...) part is a submodule
; i.e. #lang typed/racket already defines a module using typed/racket
;racket helper functions to load files as binary data
(module binfile typed/racket
  (provide (all-defined-out))

  (: binfile->list (-> Path-String (Listof Byte)))
  (define (binfile->list path)
    (call-with-input-file* path
      (λ
          ([p : Input-Port])
        (letrec ([loop : (-> Input-Port (Listof Byte) (Listof Byte))
                       (λ ([p   : Input-Port]
                           [lst : (Listof Byte)])
                         (match (read-byte p)
                           [(? byte? b)     (loop p (cons b lst))]
                           [(? eof-object?) lst])) ])
          (loop p '())))))

  (: binfile->vector (-> Path-String (Vectorof Byte)))
  (define (binfile->vector path)
    (list->vector (reverse (binfile->list path))))

  (: binfile->bytes (-> Path-String Bytes))
  (define (binfile->bytes path)
    (file->bytes path))

  (: bytes->vector (-> Bytes (Vectorof Byte)))
  (define (bytes->vector b)
    (list->vector (bytes->list b)))

  (: vector->bytes (-> (Vectorof Byte) Bytes))
  (define (vector->bytes v)
    (list->bytes (vector->list v))))

