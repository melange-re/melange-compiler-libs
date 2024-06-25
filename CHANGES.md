0.3.0 2024-06-24
--------------

- Handle EOF from OpenSSL more gracefully
  ([#23](https://github.com/anmonteiro/eio-ssl/pull/23))

0.2.0 2023-06-12
--------------

- Adapt to `eio` v0.10 ([#14](https://github.com/anmonteiro/eio-ssl/pull/14),
  [#15](https://github.com/anmonteiro/eio-ssl/pull/15))

0.1.1 2023-03-19
--------------

- Set file descriptors in non-blocking mode
  ([#12](https://github.com/anmonteiro/eio-ssl/pull/12))
  - OpenSSL requires file descriptors to be in non-blocking mode to avoid
    blocking the entire OCaml domain

0.1.0 2022-10-21
--------------

- Initial public release
