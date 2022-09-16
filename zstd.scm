(import chicken.foreign
        chicken.port
        (only chicken.memory move-memory!)
        (only chicken.io read-string! write-string)
        (only chicken.gc set-finalizer!)
        (only chicken.memory.representation number-of-bytes)
        (only chicken.blob blob?))

(foreign-declare "
#include <zstd.h>
")

(define (zstd-error? status)
  (if (zero? ((foreign-lambda unsigned-int "ZSTD_isError" size_t) status))
      status
      (error "zstd error: " ((foreign-lambda c-string "ZSTD_getErrorName" size_t) status))))

(define zstd-version (list
                      (foreign-value "ZSTD_VERSION_MAJOR" int)
                      (foreign-value "ZSTD_VERSION_MINOR" int)
                      (foreign-value "ZSTD_VERSION_RELEASE" int)))

(define (zstd-frame-content-size z)
  (let ((framesize ((foreign-lambda
                     unsigned-integer64 "ZSTD_getFrameContentSize" scheme-pointer size_t)
                    z (number-of-bytes z))))
    (if (= framesize (foreign-value "ZSTD_CONTENTSIZE_UNKNOWN" unsigned-integer64))
        #f
        (zstd-error? framesize))))

(define (zstd-compress s #!key (level 3))
  (unless (or (string? s) (blob? s)) (error 'zstd-compress "s must be string or blob" s))
  (let* ((zlen ((foreign-lambda*
                 size_t ((size_t src))
                 "return(ZSTD_COMPRESSBOUND(src));")
                (number-of-bytes s)))
         (z (make-string zlen))
         (len ((foreign-lambda size_t "ZSTD_compress"
                               scheme-pointer ;; dst
                               size_t    ;; dst capacity
                               scheme-pointer ;; src
                               size_t ;; src len
                               int #;level )
               z (number-of-bytes z)
               s (number-of-bytes s)
               level)))
    (zstd-error? len)
    (substring z 0 len)))

(define (zstd-decompress s)
  (unless (or (string? s) (blob? s)) (error 'zstd-compress "s must be string or blob" s))
  (let* ((dst (make-string (zstd-frame-content-size s)))
         (len ((foreign-lambda size_t "ZSTD_decompress"
                               scheme-pointer ;; dst
                               size_t    ;; dst capacity
                               scheme-pointer ;; src
                               size_t ;; src len
                               )
               dst (number-of-bytes dst)
               s   (number-of-bytes s))))
    (zstd-error? len)
    dst))

(define-record zstd-cstream pointer)
(define-foreign-type ZSTD_CStream*   (c-pointer "ZSTD_CStream")
  (lambda (zs) (zstd-cstream-pointer zs))
  (lambda (p) (make-zstd-cstream p)))

(define-record zstd-dstream pointer)
(define-foreign-type ZSTD_DStream*   (c-pointer "ZSTD_DStream")
  (lambda (zs) (zstd-dstream-pointer zs))
  (lambda (p) (make-zstd-dstream p)))

(define (zstd-cstream-free zs)
  (when (zstd-cstream-pointer zs)
    ((foreign-lambda size_t "ZSTD_freeCStream" ZSTD_CStream*) zs)
    (zstd-cstream-pointer-set! zs #f)))

(define (zstd-dstream-free zs)
  (when (zstd-dstream-pointer zs)
    ((foreign-lambda size_t "ZSTD_freeDStream" ZSTD_DStream*) zs)
    (zstd-dstream-pointer-set! zs #f)))

(define (new-zstd-cstream #!key
                          (level 3)
                          (finalizer (lambda (x)
                                       (set-finalizer! x zstd-cstream-free))))
  (let ((zs ((foreign-lambda ZSTD_CStream* ZSTD_createCStream))))
    ((foreign-lambda size_t "ZSTD_initCStream" ZSTD_CStream* int) zs level)
    (finalizer zs)))

(define (new-zstd-dstream #!key
                          (finalizer (lambda (x)
                                       (set-finalizer! x zstd-dstream-free))))
  (let ((zs ((foreign-lambda ZSTD_DStream* ZSTD_createDStream))))
    ((foreign-lambda size_t "ZSTD_initDStream" ZSTD_DStream*) zs)
    (finalizer zs)))

(define (zstd-cstream-compress zstream out out-pos in in-pos endOp)

  (define compressStream2
    (foreign-lambda* size_t ((ZSTD_CStream* zs)
                             (scheme-pointer out) (size_t out_len) ((c-pointer size_t) out_pos)
                             (scheme-pointer in)  (size_t  in_len) ((c-pointer size_t)  in_pos)
                             (int endOp))
                     "ZSTD_outBuffer bo = { .dst = out, .size = out_len, .pos = *out_pos};"
                     "ZSTD_inBuffer  bi = { .src =  in, .size =  in_len, .pos =  *in_pos};"
                     "size_t status = ZSTD_compressStream2(zs, &bo, &bi, endOp);"
                     "*out_pos = bo.pos;"
                     " *in_pos = bi.pos;"
                     "return(status);"))

  (let-location ((out_pos size_t out-pos)
                 ( in_pos size_t  in-pos))
    (let* ((endOp (case endOp
                    ((continue) (foreign-value "ZSTD_e_continue" int))
                    ((flush)    (foreign-value "ZSTD_e_flush"    int))
                    ((end)      (foreign-value "ZSTD_e_end"      int))
                    (else (error "endOp must be one of (continue flush end)" endOp))))
           (status (compressStream2 zstream
                                    out (number-of-bytes out) (location out_pos)
                                    in  (number-of-bytes  in) (location  in_pos)
                                    endOp)))
      (zstd-error? status)
      (values status out_pos in_pos))))

(define (compressing-output-port #!optional (output-port (current-output-port))
                                 #!key
                                 (level 3) ;; official default
                                 (buffer (make-string (* 1024 4))))

  (let ((zs (new-zstd-cstream level: level)))

    (define (feed! str type)
      (let loop ((ipos 0))
        (receive (status opos ipos) (zstd-cstream-compress zs buffer 0 str ipos type)
          (write-string buffer opos output-port)
          (unless (zero? status)
            (loop ipos)))))

    (make-output-port (lambda (str) (feed! str 'continue)) ;; write
                      (lambda ()    (feed! ""  'end))      ;; close
                      (lambda ()    (feed! ""  'flush))))) ;; flush

(define (zstd-dstream-decompress zds out out-len out-pos in in-len in-pos)

  (when (> out-pos (number-of-bytes out))
    (error "internal error, buffer overflow" out-pos out-len))

  (let-location ((out_pos size_t out-pos)
                 ( in_pos size_t in-pos))
    (let* ((status ((foreign-lambda*
                     size_t ((ZSTD_DStream* zds)
                             (scheme-pointer out) (size_t out_len) ((c-pointer size_t) out_pos)
                             (scheme-pointer in)  (size_t in_len)  ((c-pointer size_t) in_pos))
                     "ZSTD_outBuffer bo = { .dst = out+*out_pos, .size = out_len, .pos = 0};"
                     "ZSTD_inBuffer  bi = { .src =  in, .size =  in_len, .pos =  *in_pos};"
                     "size_t status = ZSTD_decompressStream(zds, &bo, &bi);"
                     "*out_pos += bo.pos;"
                     "*in_pos = bi.pos;"
                     "return(status);")
                    zds
                    out out-len (location out_pos)
                    in in-len   (location  in_pos))))
      (zstd-error? status)
      (values status out_pos in_pos))))

(define (decompressing-input-port #!optional (ip (current-input-port))
                                  #!key (buffer (make-string (* 1024 4))))

  (let ((z (new-zstd-dstream))
        (eif? #f)
        (ipos 0)
        (iend 0))

    ;; returns bytes read (0 for eof)
    (define (read! dst len opos1)
      (if (zero? len)
          0 ;; otherwise we get "Destination buffer is too small"
          (let ((oend (+ opos1 len)))
            (let loop ((opos opos1))
              (receive (status opos ipos0)
                  (zstd-dstream-decompress z dst (- oend opos) opos
                                           buffer iend ipos)
                (set! ipos ipos0)
                (if (< opos oend) ;; <-- need more data?
                    (if eif?
                        (- opos opos1)
                        (begin
                          (when (>= iend (number-of-bytes buffer)) ;; make room for more input
                            (when (= 0 ipos) (error "cannot make room " opos oend ipos iend))
                            ;; move unread compressed data down to 0
                            (move-memory! buffer buffer (- iend ipos) ipos 0)
                            (set! iend (- iend ipos))
                            (set! ipos 0))
                          ;; fill compressed buffer with as much as possible
                          (let ((read (read-string! (number-of-bytes buffer) buffer ip iend)))

                            (set! iend (+ iend read))
                            (if (zero? read) ;; ip reached eof
                                (set! eif? #t))
                            (loop opos))))
                    (- opos opos1)))))))

    (let ((eof? #f))
      (let* ((buff (make-string 1))
             (read-char (lambda ()
                          (cond (eof? #!eof)
                                ((> (read! buff 1 0) 0)
                                 (string-ref buff 0))
                                (else #!eof))))
             (peek #f)
             (zip
              (make-input-port
               ;; read-char
               (lambda ()
                 (if peek
                     (let ((char peek))
                       (set! peek #f)
                       char)
                     (read-char)))
               ;; char-ready?
               (lambda () #t)
               ;; close
               (lambda ()
                 (set! eof? #t)
                 (when z (zstd-dstream-free z))
                 (set! z #f)
                 (set! buffer "")
                 #f)
               ;; peek-char
               (lambda ()
                 (unless peek (set! peek (read-char)))
                 (when (eq? #!eof peek) (set! eof? #t))
                 peek)
               ;; read-string!
               (lambda (port len str offset)
                 (if eof? 0
                     (if peek
                         (begin (string-set! str offset peek)
                                (set! peek #f)
                                (+ 1 (read! str (- len 1) (+ offset 1))))
                         (read! str len offset)))))))
        (set-port-name! zip "(unzstd)")
        zip))))
