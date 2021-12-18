
(let loop ((n 0))
  (print "hello world " n)
  (when (< n 1000000)
    (loop (+ n 1))))
