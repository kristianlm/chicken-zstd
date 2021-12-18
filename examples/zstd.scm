(import zstd chicken.io chicken.port chicken.process-context)

(define level 
  (if (pair? (command-line-arguments))
      (- (with-input-from-string (car (command-line-arguments)) read))
      3))

(current-output-port (compressing-output-port (current-output-port) level: level))
(port-for-each write-string (lambda () (read-string (* 1024 4))))
(close-output-port (current-output-port))
