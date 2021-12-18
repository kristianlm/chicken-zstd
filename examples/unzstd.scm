(import zstd chicken.io chicken.port)

(current-input-port (decompressing-input-port (current-input-port)))
(port-for-each write-string (lambda () (read-string (* 1024 4))))

