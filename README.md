# fmemcpy

Wrap the C library function `memcpy` for Fortran.
This relies on the Fortran 2018 standard features `TYPE(*)` and `DIMENSION(..)`.

## Building

To build the wrapper run `make` from the root of the project directory.
Currently only the `gfortran` compiler has been tried.

## Testing

A simple test is included unders `tests/test.f90` which also demonstrates the use of `memcpy`.
This populates 3 1-D arrays with `integer/int`, `real(kind(0.0))/float` and
`real(kind(0.0d0))/double` data and calls the `memcpy` interface to copy the data into the
corresponding source destinations, printing the source and destination data to screen.
To build the test run
``
make test
``
and this should create an executable `test` that can be run to check the operation.
