(import zstd test
        chicken.port
        (only chicken.string conc)
        (only chicken.io read-string)
        (only chicken.process with-input-from-pipe)
        (only chicken.blob blob->string))

(test-group
 "decompression"

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
 "compression"

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
