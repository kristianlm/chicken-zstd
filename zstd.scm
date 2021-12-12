(import chicken.foreign
        chicken.port
        (only chicken.io read-string)
        (only chicken.gc set-finalizer!)
        (only chicken.memory.representation number-of-bytes)
        srfi-4)

(foreign-declare "
#include <zstd.h>
")

(define-record zstd-stream pointer port)
(define-foreign-type ZSTD_CStream*   (c-pointer "ZSTD_CStream")
  (lambda (zs) (zstd-stream-pointer zs))
  (lambda (p) (make-zstd-stream p #f)))
(define-foreign-type ZSTD_outBuffer* (c-pointer "ZSTD_outBuffer"))
(define-foreign-type ZSTD_inBuffer*  (c-pointer "ZSTD_inBuffer"))

(define (zstd-stream-free zs)
  (when (zstd-stream-pointer zs)
    ;; (print "freeing " (zstd-stream-pointer zs))
    ((foreign-lambda size_t ZSTD_freeCStream ZSTD_CStream*) zs)
    (zstd-stream-pointer-set! zs #f)))

(define (zstd-stream* #!key
                     (finalizer (lambda (x)
                                 (set-finalizer! x zstd-stream-free))))
  (finalizer ((foreign-lambda ZSTD_CStream* ZSTD_createCStream))))

;; typedef struct ZSTD_inBuffer_s {
;;   const void* src;    /**< start of input buffer */
;;   size_t size;        /**< size of input buffer */
;;   size_t pos;         /**< position where reading stopped. Will be updated. Necessarily 0 <= pos <= size */
;; } ZSTD_inBuffer;
;; typedef struct ZSTD_outBuffer_s {
;;   void*  dst;         /**< start of output buffer */
;;   size_t size;        /**< size of output buffer */
;;   size_t pos;         /**< position where writing stopped. Will be updated. Necessarily 0 <= pos <= size */
;; } ZSTD_outBuffer;

(define zstd-stream-init* (foreign-lambda size_t "ZSTD_initCStream" ZSTD_CStream* int))

(define zstd-version (list  
                      (foreign-value "ZSTD_VERSION_MAJOR" int)
                      (foreign-value "ZSTD_VERSION_MINOR" int)
                      (foreign-value "ZSTD_VERSION_RELEASE" int)))

(define (zstd-error? ret)
  (if (zero? ((foreign-lambda unsigned-int "ZSTD_isError" size_t) ret))
      ret
      (error "zstd error: " ((foreign-lambda c-string "ZSTD_getErrorName" size_t) ret))))

(define (zstd-stream-compress* zstream u8out in endOp)
  (let-location ((out_pos size_t 0))
    (let* ((endOp (case endOp
                    ((continue) (foreign-value "ZSTD_e_continue" int))
                    ((flush)    (foreign-value "ZSTD_e_flush"    int))
                    ((end)      (foreign-value "ZSTD_e_end"      int))
                    (else (error "endOp must be one of (continue flush end)" endOp))))
           (ret ((foreign-lambda* size_t ((ZSTD_CStream* zs)
                                          (u8vector out) (size_t out_len)
                                          (scheme-pointer in)  (size_t in_len)
                                          (int endOp)
                                          ((c-pointer size_t) out_pos))
                                  "ZSTD_outBuffer bo = { .dst = out, .size = out_len, .pos = 0};"
                                  "ZSTD_inBuffer  bi = { .src =  in, .size =  in_len, .pos = 0};"
                                  "size_t ret = ZSTD_compressStream2(zs, &bo, &bi, endOp);"
                                  "*out_pos = bo.pos;"
                                  "return(ret);")
                 zstream
                 u8out (u8vector-length u8out)
                 in  (number-of-bytes  in)
                 endOp
                 (location out_pos))))
      (zstd-error? ret)
      ;; (print "RET RET " ret " out_pos = " out_pos)
      out_pos)))

(define (zstd-stream-compressed output-port
                                #!key
                                (level 3) ;; default
                                (output-buffer (make-u8vector (* 1024 1024 1))))
  (let ((zs (zstd-stream*)))
    (zstd-stream-init* zs level)
    (zstd-stream-port-set! zs output-port)
    (make-output-port
     ;; write
     (lambda (str)
       ;;(display "write\n" (current-error-port))
       (let ((len (zstd-stream-compress* zs output-buffer str 'continue)))
         (write-u8vector output-buffer output-port 0 len)))
     ;; close
     (lambda ()
       (let loop ()
         ;;(display "close\n" (current-error-port))
         (let ((len (zstd-stream-compress* zs output-buffer "" 'end)))
           (write-u8vector output-buffer output-port 0 len))))
     ;; flush
     (lambda ()
       ;;(display "flush\n" (current-error-port))
       ;; TODO: loop here if needed
       (zstd-stream-compress* zs output-buffer "" 'flush)))))

;; (define zstd-stream-end      (foreign-lambda size_t "ZSTD_endStream" ZSTD_CStream* ZSTD_outBuffer*))

(define z (zstd-stream-compressed (current-output-port) level: 19))

(port-for-each
 (lambda (str) (display str z))
 (lambda () (read-string (* 4 1024 1024))))

;; `(call-with-output-file "/tmp/hello.zst"
;;   (lambda (of)
;;     (let ((zp (zstd-stream-compressed of level: 19)))
;;       (print "zp is " zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (display "hello world" zp)
;;       (close-output-port zp))))
