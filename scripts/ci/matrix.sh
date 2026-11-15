#!/usr/bin/env bash
# forbear's tests on one toolchain, run by .github/workflows/matrix.yml on each toolchain of the fobos [scaffold] matrix
# (gfortran, ifx, nvfortran in the fortran-toolchains images), with the environment contract: FC, CC, CXX, TOOLCHAIN,
# TOOLCHAIN_OS and TOOLCHAIN_FAMILY (gnu, nvf, intel: the fobos modes tests-<family>-debug). Created by fobis scaffold,
# then the project's. The same script runs locally, in `docker run <image>` and in CI.
set -euo pipefail
echo "toolchain: $TOOLCHAIN ($TOOLCHAIN_OS, $TOOLCHAIN_FAMILY), FC=$FC"
fobis fetch --no-build
fobis build --mode "tests-$TOOLCHAIN_FAMILY-debug"
bash scripts/run_tests.sh
