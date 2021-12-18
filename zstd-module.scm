(module zstd (zstd-version
              compressing-output-port
              decompressing-input-port

              ;; exporting these secretly in case they are needed
              zstd-cstream new-zstd-cstream zstd-cstream? zstd-cstream-free zstd-cstream-compress
              zstd-dstream new-zstd-dstream zstd-dstream? zstd-dstream-free zstd-dstream-decompress)
(import scheme chicken.base)
(include "zstd.scm"))
