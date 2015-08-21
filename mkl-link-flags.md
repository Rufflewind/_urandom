MKL SDL

    -fopenmp -lmkl_rt -lpthread -lm

MKL ST

    -lmkl_intel_lp64 -lmkl_core -lmkl_sequential -lpthread -lm

MKL MT

    -fopenmp -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm

Using Armadillo-4.600+ with MKL on HPC
--------------------------------------

```sh
module add MKL
cmake -DCMAKE_INSTALL_PREFIX="$HOME" .
make install
```
