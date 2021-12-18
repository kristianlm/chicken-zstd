
  [Chicken Scheme]: http://call-cc.org
  [zstd]: http://www.zstd.net/

# [zstd] bindings for [Chicken Scheme]

## Requirements

- [zstd] version 1.4.0+.

## Source Code

Hosted [here](https://github.com/kristianlm/chicken-zstd).

## API

    [procedure] (compressing-output-port output-port #!key (level 3)) => output-port

Returns an output-port to which uncompressed data can be written, and
its compressed form will be written to `output-port`. If `level` is
supplied, it sets the compression level as documented by `man zstd`,
and should be an integer in the range [1-19]. Closing this output-port
does not close `output-port`.

    [procedure] (decompressing-input-port input-port) => input-port

Returns an input-port from which uncompressed data can be
read. Compressed data read, from `input-port`, will be read in the
process. Closing this input-port does not close `input-port`.

## API Design

The API surface is intentionally left small, with only 2 port-based
functions. Hopefully, this is sufficient for most use-cases. If this
is not the case, please give feedback.

## Examples

Some very basic usage for illustration.

```scheme
(import zstd chicken.process chicken.io chicken.port)

(with-input-from-pipe ;; => "hello hello hello\n"
 "echo hello hello hello | zstd"
 (lambda () (with-input-from-port (decompressing-input-port) read-string)))

(with-output-to-pipe ;; prints "hello world" to stdout
 "zstd -d"
 (lambda ()
   (current-output-port (compressing-output-port))
   (print "hello world")
   (close-output-port (current-output-port))))
```

## Example code for benchmarking and testing

The code under `./examples` may be useful to try things out and
compare against the `zstd` command-line-tool.

```sh
> cd examples
> make
csc hello.scm
csc zstd.scm
csc unzstd.scm
> ./hello | pv -cNu | sha1sum
        u: 18.0MiB 0:00:00 [47.2MiB/s]
7c363606a232e640bb0f9dce8b0023a78f7059ba  -
> ./hello | pv -cNu | ./zstd -1 | pv -cNz | ./unzstd | sha1sum
        u: 18.0MiB 0:00:00 [46.2MiB/s]
        z:  343KiB 0:00:00 [ 879KiB/s]
7c363606a232e640bb0f9dce8b0023a78f7059ba  -
> ./hello | pv -cNu | ./zstd -22 | pv -cNz | ./unzstd | sha1sum
        u: 18.0MiB 0:00:22 [ 828KiB/s]
        z:  115KiB 0:00:22 [5.15KiB/s]
7c363606a232e640bb0f9dce8b0023a78f7059ba  -
> ./hello | pv -cNu | zstd -1 | pv -cNz | unzstd | sha1sum
        u: 18.0MiB 0:00:00 [46.5MiB/s]
        z:  486KiB 0:00:00 [1.22MiB/s]
7c363606a232e640bb0f9dce8b0023a78f7059ba  -
```


## Development status

- This egg does not support the dictionary features of the zstd C API.
- This egg does not support re-using `z-stream` contexts.
- This egg does not support the simple API, only the "unbounded"
  streaming API. This may be less efficient for small data.
