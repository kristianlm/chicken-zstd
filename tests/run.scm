(import zstd test
        chicken.port
        (only chicken.io read-string)
        (only chicken.blob string->blob))

(test-group "frame-content-size"
            (test "empty frame"  0 (zstd-frame-content-size (zstd-compress "")))
            (test "small frame"  5 (zstd-frame-content-size (zstd-compress "hello")))
            (test "big frame" 4096 (zstd-frame-content-size (zstd-compress (make-string 4096 #\x))))

            (test-error "frame-content-size on invalid frame" (zstd-frame-content-size ""))


            (let ((frame (with-output-to-string
                           (lambda ()
                             (let ((p (compressing-output-port)))
                               (display "test" p)
                               (close-output-port p))))))
              (test "frame-content-size gives #f on stream API frames"
                    #f
                    (zstd-frame-content-size frame))))

(test-group
 "simple API"
 (define hllo "(\xb5/\xfd \x05)\x00\x00hello") ;; compressed hello
 (test-error "type check compress" (zstd-compress 12))
 (test-error "type check decompress" (zstd-compress 12))
 (test "compress hello string" hllo (zstd-compress "hello"))
 (test "compress hello blob" hllo (zstd-compress (string->blob "hello")))

 (define aaa... "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
 (test "compression level" #t (> (string-length (zstd-compress aaa... #:level 1))
                                 (string-length (zstd-compress aaa... #:level 19))))
 
 (test "decompress hello string" "hello" (zstd-decompress hllo))
 (test "decompress hello blob" "hello" (zstd-decompress (string->blob hllo)))


 (test "" ;; make sure frame-size 0 ≠ error
       (zstd-decompress (zstd-compress "")))
 (test-error "decompress empty string" (zstd-decompress "")) )

(test-group
 "streaming decompression"

 (define (wifdip proc)
   (with-input-from-port
       (decompressing-input-port ;;                  ,-- didn't compress well
        (open-input-string "(\265/\375\x04hy\x00\x00abcdefghjiklmn\n\340M\332\342"))
     proc))

 (test "read all" "abcdefghjiklmn\n" (wifdip (lambda () (read-string #f))))

 (test "r"   #\a (wifdip (lambda () (read-char))))
 (test "rr"  #\b (wifdip (lambda () (read-char) (read-char))))
 (test "rrr" #\c (wifdip (lambda () (read-char) (read-char) (read-char))))

 (define peeked #f)
 (test "rpr" #\b (wifdip (lambda () (read-char) (set! peeked (peek-char)) (read-char))))
 (test "peeked" #\b peeked)

 (test "r s1" "b"    (wifdip (lambda () (read-char) (read-string 1))))
 (test "r s4" "bcde" (wifdip (lambda () (read-char) (read-string 4))))
 (test "r s1k" "bcdefghjiklmn\n" (wifdip (lambda () (read-char) (read-string 1024))))
 (test "p s1k" "abcdefghjiklmn\n" (wifdip (lambda () (peek-char) (read-string 1024))))

 (with-input-from-port
     (decompressing-input-port
      (open-input-string "(\265/\375\x04hy\x00\x00abcdefghjiklmn\n\340M\332\342")
      buffer: (make-string 1)) ;; triger refills
   (lambda ()
     (test #\a (read-char))
     (test "b" (read-string 1))
     (test "cdefghjiklmn\n" (read-string 1024))
     (test "eof" #!eof (read-string #f))))

 (with-input-from-port
     (decompressing-input-port
      (open-input-string "(\265/\375\x04hy\x00\x00abcdefghjiklmn\n\340M\332\342"))
   (lambda ()
     (test "ab" (read-string 2))
     (close-input-port (current-input-port))
     (test-error "read after close" (read-string 1)))))

(test-group
 "streaming compression"

 (test "basic port" #t (output-port? (compressing-output-port)))

 (define (with-output-to-compressed-string-and-back thunk)
   (let ((compressed (with-output-to-string
                       (lambda ()
                         (define cp (compressing-output-port (current-output-port)))
                         (with-output-to-port cp thunk)
                         (close-output-port cp)))))
     (read-string #f (decompressing-input-port
                      (open-input-string compressed)))))
 (define wotcsab with-output-to-compressed-string-and-back)

 (test "no output" #!eof (wotcsab (lambda () #f)))
 (test "\"a\"" "a" (wotcsab (lambda () (display "a"))))
 (test "hello world" "hello world\n" (wotcsab (lambda () (print "hello world"))))
 (test "hello world" "abc" (wotcsab (lambda () (display "a") (display "b") (display "c"))))

 (test "big string"
       (values                      (make-string (* 1024 1024) #\x))
       (wotcsab (lambda () (display (make-string (* 1024 1024) #\x)))))

 (define (spam)
   (let loop ((n 0))
     (print "isd83jdgvlhjdu4cvukitsq1aryj " n)
     (when (< n 10000)
       (loop (+ n 1)))))

 ;; I want to trigger when zstd-dstream-decompress cant consume all input, ie (≠ ipos (number-of-bytes str))
 (test "small transit buffer and lots of data"
       (with-output-to-string spam)
       (read-string
        #f (decompressing-input-port
            (open-input-string
             (with-output-to-string
               (lambda ()
                 (with-output-to-port (compressing-output-port (current-output-port) buffer: " ")
                   (lambda ()
                     (spam)
                     (close-output-port (current-output-port)))))))))))

(test-exit)
