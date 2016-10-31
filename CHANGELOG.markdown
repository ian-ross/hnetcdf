0.3.0.1
-------
* Update to work with hmatrix 0.17 -- `CFloat` and `CDouble` are not longer supported
  in hmatrix-0.17. It is possible to use hmatrix by converting `CFloat/CDouble` to
  `Float/Double` using `smap realToFrac` (see examples/example3.hs and test/test-get.hs).

0.3.0.0
-------
* Breaking change: fix problem with hmatrix API -- replace
  `HRowMajorMatrix` and `HColumnMajorMatrix` with single `HMatrix`
  type and manage data ordering internally.

0.2.1.0
-------
* Update to work with hmatrix 0.16

0.2.0.0
-------
* Add partial output API
