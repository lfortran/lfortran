#!/bin/bash
set -ex  # Exit immediately on any error

# Default to gfortran if FC is not set
: "${FC:=gfortran}"

# Color definitions for pretty output
GREEN='\033[0;32m'
BLUE='\033[1;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions for logging
print_section() {
  echo -e "\n${BLUE}==============================="
  echo -e "$1"
  echo -e "===============================${NC}\n"
}

print_subsection() {
  echo -e "${YELLOW}→ $1${NC}"
}

print_success() {
  echo -e "${GREEN}✔ $1${NC}"
}

run_test() {
  print_subsection "Running: $1"
  ./$1
  print_success "Success: $1"
}

time_section() {
  local LABEL="$1"
  local BLOCK="$2"
  local START=$(date +%s)
  print_section "$LABEL"
  eval "$BLOCK"
  local END=$(date +%s)
  print_subsection "⏱ Duration: $((END - START)) seconds"
}

# Setup a temporary workspace
TMP_DIR=$(mktemp -d)
cd "$TMP_DIR"

time_section "🧪 Testing assert" '
  git clone https://github.com/pranavchiku/assert.git
  cd assert
  export PATH="$(pwd)/../src/bin:$PATH"
  micromamba install -c conda-forge fpm

  # To debug https://github.com/lfortran/lfortran/issues/7732:
  which fpm
  realpath $(which fpm)
  ls -l $(dirname $(realpath $(which fpm)))/../lib
  ls -l $CONDA_PREFIX/lib
  fpm --version

  git checkout -t origin/fix-test
  git checkout 535434d2f44508aa06231c6c2fe95f9e11292769
  git clean -dfx
  fpm build --compiler=$FC --flag "--cpp" --verbose
  fpm test --compiler=$FC --flag "--cpp"

  git clean -dfx
  print_subsection "Testing with assertions enabled"
  fpm test --compiler=$FC --verbose --flag '--cpp -DASSERTIONS -DASSERT_PARALLEL_CALLBACKS'

  cd ../
  rm -rf assert
'


time_section "🧪 Testing splpak" '
  git clone https://github.com/Pranavchiku/splpak.git
  cd splpak
  export PATH="$(pwd)/../src/bin:$PATH"
  micromamba install -c conda-forge fpm

  # To debug https://github.com/lfortran/lfortran/issues/7732:
  which fpm
  realpath $(which fpm)
  ls -l $(dirname $(realpath $(which fpm)))/../lib
  ls -l $CONDA_PREFIX/lib
  fpm --version

  git checkout lf-2
  git checkout 460bd22f4ac716e5266412e8ed35ce07aa664f08

  git clean -dfx
  fpm build --compiler=$FC --profile release --flag "--cpp -DREAL32" --verbose
  fpm test --compiler=$FC --profile release --flag "--cpp -DREAL32"

  cd ../
  rm -rf splpak
'

time_section "🧪 Testing fortran-regex" '
  git clone https://github.com/perazz/fortran-regex.git
  cd fortran-regex
  export PATH="$(pwd)/../src/bin:$PATH"
  micromamba install -c conda-forge fpm

  git checkout 96ab33fe003862a28cec91ddd170ac0e86c26c87
  fpm --compiler=$FC build

  print_success "Done with fortran-regex"
  cd ..
'

time_section "🧪 Testing fortran-shlex" '
  git clone https://github.com/jinangshah21/fortran-shlex.git
  cd fortran-shlex
  export PATH="$(pwd)/../src/bin:$PATH"
  git checkout lf-1
  micromamba install -c conda-forge fpm
  git checkout a030f1b9754ac3e6c5aa17fed01e5c2d767b947b
  fpm --compiler=$FC build
  print_success "Done with fortran-shlex"
  cd ..
'

time_section "🧪 Testing toml-f" '
  git clone https://github.com/jinangshah21/toml-f.git
  cd toml-f
  export PATH="$(pwd)/../src/bin:$PATH"
  git checkout lf-2
  micromamba install -c conda-forge fpm
  git checkout 38c294fc5d9cb5fa6a9d3569514da28b8705f3fb
  fpm --compiler=$FC build
  print_success "Done with toml-f"
  cd ..
'

time_section "🧪 Testing jonquil" '
  git clone https://github.com/jinangshah21/jonquil.git
  cd jonquil
  export PATH="$(pwd)/../src/bin:$PATH"
  git checkout lf-1
  micromamba install -c conda-forge fpm
  git checkout c5a272475772526343cf1d6864411b233c7d7c05
  fpm --compiler=$FC build
  print_success "Done with jonquil"
  cd ..
'

time_section "🧪 Testing M_CLI2" '
  git clone https://github.com/jinangshah21/M_CLI2.git
  cd M_CLI2
  export PATH="$(pwd)/../src/bin:$PATH"
  git checkout lf-2
  micromamba install -c conda-forge fpm
  git checkout c34cfdde5e1f82918668c5d06c3faa315489b1c0
  fpm --compiler=$FC build
  print_success "Done with M_CLI2"
  cd ..
'

time_section "🧪 Testing fortran_mpi" '
  git clone https://github.com/lfortran/fortran_mpi.git
  cd fortran_mpi
  export PATH="$(pwd)/../src/bin:$PATH"

  git checkout 31033d3c8af32c4c99fac803c161e6731bc39a78

  git clean -fdx
  cd tests/
  FC="$FC --cpp" ./run_tests.sh
  print_success "Done with fortran_mpi"

  cd ../
  git clean -fdx
  print_subsection "Building fortran_mpi with separate compilation"
  cd tests/
  FC="$FC --cpp --separate-compilation" ./run_tests.sh
  print_success "Done with fortran_mpi"
  cd ../../
  rm -rf fortran_mpi
'

time_section "🧪 Testing POT3D with fortran_mpi" '
  git clone https://github.com/gxyd/pot3d.git
  cd pot3d
  git checkout -t origin/lf_hdf5_fortranMPI_namelist_global_workarounds
  git checkout 9bf5d4784581ce83e2df13b828de86950ba88902

  git clone https://github.com/lfortran/fortran_mpi
  cd fortran_mpi
  git checkout 31033d3c8af32c4c99fac803c161e6731bc39a78
  cp src/mpi.f90 ../src/
  cp src/mpi_c_bindings.f90 ../src/
  cp src/mpi_constants.c ../src/
  cd ..

  print_subsection "Building with default flags"
  FC="$FC --cpp -DOPEN_MPI=yes --no-array-bounds-checking" ./build_and_run_lfortran.sh

  print_subsection "Building with optimization flags"
  # FC="$FC --cpp --fast --skip-pass=dead_code_removal -DOPEN_MPI=yes" ./build_and_run_lfortran.sh

  print_subsection "Building POT3D in separate compilation mode"
  FC="$FC --cpp --separate-compilation --no-array-bounds-checking -DOPEN_MPI=yes" ./build_and_run_lfortran.sh

  print_success "Done with POT3D"
  cd ..
  rm -rf pot3d
'


##########################
# Section 1: stdlib (Less Workarounds)
##########################
time_section "🧪 Testing stdlib (Less Workarounds)" '
  git clone https://github.com/Pranavchiku/stdlib-fortran-lang.git
  cd stdlib-fortran-lang
  export PATH="$(pwd)/../src/bin:$PATH"

  git checkout n-lf-20
  git checkout 8cbc69ca553b5c1b3d5bd882263e108feb75fa7a
  micromamba install -c conda-forge fypp

  git clean -fdx
  FC=$FC cmake . \
      -DTEST_DRIVE_BUILD_TESTING=OFF \
      -DBUILD_EXAMPLE=ON -DCMAKE_Fortran_COMPILER_WORKS=TRUE \
      -DCMAKE_Fortran_FLAGS="--cpp --realloc-lhs --no-warnings --use-loop-variable-after-loop -I$(pwd)/src -I$(pwd)/subprojects/test-drive/"
  make -j8
  ctest

  git clean -dfx
  git restore .
  git checkout sc-lf-11
  git checkout 76e15075b73b5bab39adf24b70fe15973069dee2
  FC=$FC cmake . \
      -DTEST_DRIVE_BUILD_TESTING=OFF \
      -DBUILD_EXAMPLE=ON -DCMAKE_Fortran_COMPILER_WORKS=TRUE \
      -DCMAKE_Fortran_FLAGS="--cpp --separate-compilation --realloc-lhs --no-warnings --use-loop-variable-after-loop -I$(pwd)/src -I$(pwd)/subprojects/test-drive/"
  make -j8
  ctest

  print_success "Done with stdlib (Less Workarounds)"
  cd ..
  rm -rf stdlib
'

##########################
# Section 2: FPM
##########################
time_section "🧪 Testing FPM" '
  git clone https://github.com/jinangshah21/fpm.git
  cd fpm
  export PATH="$(pwd)/../src/bin:$PATH"
  git checkout lf-6
  micromamba install -c conda-forge fpm
  git checkout b946c489ef575e8103b6f5b07bfdfe0171e1dac5
  fpm --compiler=$FC build
  print_success "Done with FPM"
  cd ..
'

##########################
# Section 3: Fortran-Primes
##########################
time_section "🧪 Testing Fortran-Primes" '
  git clone https://github.com/jinangshah21/fortran-primes.git
  cd fortran-primes
  git checkout -t origin/lf-3
  git checkout 923b468f79eee1ff07b77d9def67249f4d2efa21

  print_subsection "Building and running Fortran-Primes"
  FC=$FC ./build_and_run.sh

  print_subsection "Building Fortran-Primes with separate compilation"
  git clean -dfx
  FC="$FC --separate-compilation" ./build_and_run.sh

  print_success "Done with Fortran-Primes"
  cd ..
  rm -rf fortran-primes
'

########################################
# Section 4: Numerical Methods Fortran #
########################################
time_section "🧪 Testing Numerical Methods Fortran" '
  git clone https://github.com/Pranavchiku/numerical-methods-fortran.git
  cd numerical-methods-fortran
  git checkout -t origin/lf6
  git checkout a252989e64b3f8d5d2f930dca18411c104ea85f8

  print_subsection "Building project"
  FC=$FC make

  run_test test_fix_point.exe
  run_test test_integrate_one.exe
  run_test test_linear.exe
  run_test test_newton.exe
  run_test test_ode.exe
  run_test test_probability_distribution.exe
  run_test test_sde.exe

  run_test plot_bogdanov_takens.exe
  run_test plot_bruinsma.exe
  run_test plot_fun1.exe
  run_test plot_lorenz.exe
  run_test plot_lotka_volterra1.exe
  run_test plot_lotka_volterra2.exe
  run_test plot_pendulum.exe
  run_test plot_transes_iso.exe

  git clean -dfx
  print_subsection "Building Numerical Methods Fortran with f23 standard"

  FC="$FC --std=f23" make
  run_test test_fix_point.exe
  run_test test_integrate_one.exe
  run_test test_linear.exe
  run_test test_newton.exe
  run_test test_ode.exe
  run_test test_probability_distribution.exe
  run_test test_sde.exe

  run_test plot_bogdanov_takens.exe
  run_test plot_bruinsma.exe
  run_test plot_fun1.exe
  run_test plot_lorenz.exe
  run_test plot_lotka_volterra1.exe
  run_test plot_lotka_volterra2.exe
  run_test plot_pendulum.exe
  run_test plot_transes_iso.exe


  git clean -dfx
  print_subsection "Building Numerical Methods Fortran with separate compilation"

  FC="$FC --separate-compilation" make
  run_test test_fix_point.exe
  run_test test_integrate_one.exe
  run_test test_linear.exe
  run_test test_newton.exe
  run_test test_ode.exe
  run_test test_probability_distribution.exe
  run_test test_sde.exe

  run_test plot_bogdanov_takens.exe
  run_test plot_bruinsma.exe
  run_test plot_fun1.exe
  run_test plot_lorenz.exe
  run_test plot_lotka_volterra1.exe
  run_test plot_lotka_volterra2.exe
  run_test plot_pendulum.exe
  run_test plot_transes_iso.exe

  git clean -dfx
  print_subsection "Building Numerical Methods Fortran with separate compilation and f23 standard"

  FC="$FC --separate-compilation --std=f23" make
  run_test test_fix_point.exe
  run_test test_integrate_one.exe
  run_test test_linear.exe
  run_test test_newton.exe
  run_test test_ode.exe
  run_test test_probability_distribution.exe
  run_test test_sde.exe

  run_test plot_bogdanov_takens.exe
  run_test plot_bruinsma.exe
  run_test plot_fun1.exe
  run_test plot_lorenz.exe
  run_test plot_lotka_volterra1.exe
  run_test plot_lotka_volterra2.exe
  run_test plot_pendulum.exe
  run_test plot_transes_iso.exe


  print_success "Done with Numerical Methods Fortran"

  cd ..
  rm -rf numerical-methods-fortran
'

#######################
# Section 5: PRIMA    #
#######################
time_section "🧪 Testing PRIMA" '
  git clone https://github.com/Pranavchiku/prima.git
  cd prima
  git checkout -t origin/lf-prima-12
  git checkout e681eea9b3f27930c50cffd14dd566b39f01c642
  git clean -dfx

  # OS-specific env
  if [[ "$RUNNER_OS" == "macos-latest" ]]; then
    export LFORTRAN_RUNNER_OS="macos"
  elif [[ "$RUNNER_OS" == "ubuntu-latest" ]]; then
    export LFORTRAN_RUNNER_OS="linux"
  fi

  print_subsection "Building PRIMA"
  FC="$FC --cpp" cmake -S . -B build \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/install \
    -DCMAKE_Fortran_FLAGS="" \
    -DCMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS="" \
    -DCMAKE_MACOSX_RPATH=OFF \
    -DCMAKE_SKIP_INSTALL_RPATH=ON \
    -DCMAKE_SKIP_RPATH=ON

  cmake --build build --target install

  run_test ./build/fortran/example_bobyqa_fortran_1_exe
  run_test ./build/fortran/example_bobyqa_fortran_2_exe
  run_test ./build/fortran/example_cobyla_fortran_1_exe
  run_test ./build/fortran/example_cobyla_fortran_2_exe
  run_test ./build/fortran/example_lincoa_fortran_1_exe
  run_test ./build/fortran/example_lincoa_fortran_2_exe
  run_test ./build/fortran/example_newuoa_fortran_1_exe
  run_test ./build/fortran/example_newuoa_fortran_2_exe
  run_test ./build/fortran/example_uobyqa_fortran_1_exe
  run_test ./build/fortran/example_uobyqa_fortran_2_exe

  if [[ "$RUNNER_OS" == "macos-latest" ]]; then
    cd fortran
    test_name=test_bobyqa.f90 FC="$FC" ./script.sh
    test_name=test_newuoa.f90 FC="$FC" ./script.sh
    test_name=test_uobyqa.f90 FC="$FC" ./script.sh
    test_name=test_cobyla.f90 FC="$FC" ./script.sh
    test_name=test_lincoa.f90 FC="$FC" ./script.sh
    cd ..
  fi

  if [[ "$RUNNER_OS" == "ubuntu-latest" ]]; then
    cd fortran
    test_name=test_uobyqa.f90 FC="$FC" ./script.sh
    cd ..
  fi

  print_subsection "Building PRIMA with f23 standard"
  FC="$FC --cpp --std=f23" cmake -S . -B build \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/install \
    -DCMAKE_Fortran_FLAGS="" \
    -DCMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS="" \
    -DCMAKE_MACOSX_RPATH=OFF \
    -DCMAKE_SKIP_INSTALL_RPATH=ON \
    -DCMAKE_SKIP_RPATH=ON

  cmake --build build --target install

  run_test ./build/fortran/example_bobyqa_fortran_1_exe
  run_test ./build/fortran/example_bobyqa_fortran_2_exe
  run_test ./build/fortran/example_cobyla_fortran_1_exe
  run_test ./build/fortran/example_cobyla_fortran_2_exe
  run_test ./build/fortran/example_lincoa_fortran_1_exe
  run_test ./build/fortran/example_lincoa_fortran_2_exe
  run_test ./build/fortran/example_newuoa_fortran_1_exe
  run_test ./build/fortran/example_newuoa_fortran_2_exe
  run_test ./build/fortran/example_uobyqa_fortran_1_exe
  run_test ./build/fortran/example_uobyqa_fortran_2_exe

  if [[ "$RUNNER_OS" == "macos-latest" ]]; then
    cd fortran
    test_name=test_bobyqa.f90 FC="$FC --std=f23" ./script.sh
    test_name=test_newuoa.f90 FC="$FC --std=f23" ./script.sh
    test_name=test_uobyqa.f90 FC="$FC --std=f23" ./script.sh
    test_name=test_cobyla.f90 FC="$FC --std=f23" ./script.sh
    test_name=test_lincoa.f90 FC="$FC --std=f23" ./script.sh
    cd ..
  fi

  if [[ "$RUNNER_OS" == "ubuntu-latest" ]]; then
    cd fortran
    test_name=test_uobyqa.f90 FC="$FC --std=f23" ./script.sh
    cd ..
  fi

  print_subsection "Rebuilding PRIMA with optimization"
  git clean -dfx

  FC="$FC --cpp --fast" cmake -S . -B build \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/install \
    -DCMAKE_Fortran_FLAGS="" \
    -DCMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS="" \
    -DCMAKE_MACOSX_RPATH=OFF \
    -DCMAKE_SKIP_INSTALL_RPATH=ON \
    -DCMAKE_SKIP_RPATH=ON

  cmake --build build --target install

  run_test ./build/fortran/example_bobyqa_fortran_1_exe
  run_test ./build/fortran/example_bobyqa_fortran_2_exe
  run_test ./build/fortran/example_cobyla_fortran_1_exe
  run_test ./build/fortran/example_cobyla_fortran_2_exe
  run_test ./build/fortran/example_lincoa_fortran_1_exe
  run_test ./build/fortran/example_lincoa_fortran_2_exe
  run_test ./build/fortran/example_newuoa_fortran_1_exe
  run_test ./build/fortran/example_newuoa_fortran_2_exe
  run_test ./build/fortran/example_uobyqa_fortran_1_exe
  run_test ./build/fortran/example_uobyqa_fortran_2_exe

  print_subsection "Rebuilding PRIMA in separate compilation mode"
  git clean -dfx
  git restore --staged .
  git restore .
  git checkout -t origin/lf-prima-sc-1
  git checkout 52b863fcd3bb694045e50884fbb689a1ca75298d
  FC="$FC --separate-compilation --cpp" cmake -S . -B build \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/install \
    -DCMAKE_Fortran_FLAGS="" \
    -DCMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS="" \
    -DCMAKE_MACOSX_RPATH=OFF \
    -DCMAKE_SKIP_INSTALL_RPATH=ON \
    -DCMAKE_SKIP_RPATH=ON

  cmake --build build --target install

  run_test ./build/fortran/example_bobyqa_fortran_1_exe
  run_test ./build/fortran/example_bobyqa_fortran_2_exe
  run_test ./build/fortran/example_cobyla_fortran_1_exe
  run_test ./build/fortran/example_cobyla_fortran_2_exe
  run_test ./build/fortran/example_lincoa_fortran_1_exe
  run_test ./build/fortran/example_lincoa_fortran_2_exe
  run_test ./build/fortran/example_newuoa_fortran_1_exe
  run_test ./build/fortran/example_newuoa_fortran_2_exe
  run_test ./build/fortran/example_uobyqa_fortran_1_exe
  run_test ./build/fortran/example_uobyqa_fortran_2_exe

  if [[ "$RUNNER_OS" == "macos-latest" ]]; then
    cd fortran
    name=bobyqa test_name=test_bobyqa.f90 FC="$FC --separate-compilation" ./script_sc.sh
    name=newuoa test_name=test_newuoa.f90 FC="$FC --separate-compilation" ./script_sc.sh
    name=uobyqa test_name=test_uobyqa.f90 FC="$FC --separate-compilation" ./script_sc.sh
    name=cobyla test_name=test_cobyla.f90 FC="$FC --separate-compilation" ./script_sc.sh
    name=lincoa test_name=test_lincoa.f90 FC="$FC --separate-compilation" ./script_sc.sh
    cd ..
  fi

  if [[ "$RUNNER_OS" == "ubuntu-latest" ]]; then
    cd fortran
    name=uobyqa test_name=test_uobyqa.f90 FC="$FC --separate-compilation" ./script_sc.sh
    cd ..
  fi

  print_success "Done with PRIMA"
  cd ..
'

##########################
# Section 6: Legacy Minpack
##########################
time_section "🧪 Testing Legacy Minpack (SciPy)" '
  git clone https://github.com/pranavchiku/minpack.git
  cd minpack
  git checkout -t origin/scipy31
  git checkout 45801cf882871ea8a668213e8cf90b5817877484
  mkdir lf && cd lf

  FC="$FC --intrinsic-mangling" cmake ..
  make

  run_test examples/example_hybrd
  run_test examples/example_hybrd1
  run_test examples/example_lmder1
  run_test examples/example_lmdif1
  run_test examples/example_primes
  print_subsection "Running CTest"
  ctest
  cd ../

  print_subsection "Testing with f23 standard"
  git clean -dfx
  mkdir lf && cd lf
  FC="$FC --intrinsic-mangling --std=f23" cmake ..
  make
  run_test examples/example_hybrd
  run_test examples/example_hybrd1
  run_test examples/example_lmder1
  run_test examples/example_lmdif1
  run_test examples/example_primes
  print_subsection "Running CTest"
  ctest
  cd ../

  print_subsection "Testing with separate compilation"
  git clean -dfx
  mkdir lf && cd lf
  FC="$FC --intrinsic-mangling --separate-compilation" cmake ..
  make
  run_test examples/example_hybrd
  run_test examples/example_hybrd1
  run_test examples/example_lmder1
  run_test examples/example_lmdif1
  run_test examples/example_primes
  print_subsection "Running CTest"
  ctest
  cd ../

  print_subsection "Testing with separate compilation and f23 standard"
  git clean -dfx
  mkdir lf && cd lf
  FC="$FC --intrinsic-mangling --separate-compilation --std=f23" cmake ..
  make
  run_test examples/example_hybrd
  run_test examples/example_hybrd1
  run_test examples/example_lmder1
  run_test examples/example_lmdif1
  run_test examples/example_primes
  print_subsection "Running CTest"
  ctest
  cd ../

  print_success "Done with Legacy Minpack (SciPy)"
  cd ../
  rm -rf minpack
'

##########################
# Section 7: Modern Minpack
##########################
time_section "🧪 Testing Modern Minpack (Fortran-Lang)" '
  git clone https://github.com/fortran-lang/minpack modern_minpack_01
  cd modern_minpack_01
  git checkout c0b5aea9fcd2b83865af921a7a7e881904f8d3c2

  $FC ./src/minpack.f90 -c --legacy-array-sections
  $FC ./examples/example_hybrd.f90 --legacy-array-sections
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections --no-array-bounds-checking
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections --no-array-bounds-checking
  $FC ./examples/example_lmder1.f90 --legacy-array-sections --no-array-bounds-checking

  print_subsection "Testing with separate compilation"
  git clean -dfx
  $FC ./src/minpack.f90 -c --legacy-array-sections --separate-compilation --no-array-bounds-checking
  $FC ./examples/example_hybrd.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
  $FC ./examples/example_lmder1.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
'

time_section "🧪 Testing Modern Minpack (Result Check)" '
  git clone https://github.com/Pranavchiku/modern_minpack.git modern_minpack_02
  cd modern_minpack_02
  git checkout -t origin/w5
  git checkout fcde66ca86348eb0c4012dbdf0f4d8dba61261d8

  $FC ./src/minpack.f90 -c --legacy-array-sections
  $FC ./examples/example_hybrd.f90 --legacy-array-sections
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections --no-array-bounds-checking
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections --no-array-bounds-checking
  $FC ./examples/example_lmder1.f90 --legacy-array-sections --no-array-bounds-checking

  print_subsection "Testing with separate compilation"
  git clean -dfx
  $FC ./src/minpack.f90 -c --legacy-array-sections --separate-compilation --no-array-bounds-checking
  $FC ./examples/example_hybrd.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
  $FC ./examples/example_lmder1.f90 --legacy-array-sections --separate-compilation --no-array-bounds-checking minpack.o
'

##########################
# Section 8: dftatom
##########################
time_section "🧪 Testing dftatom" '
  git clone https://github.com/certik/dftatom.git
  cd dftatom
  git checkout 9b678177f67e350b8a32e08cb61f51e6e708e87a

  make -f Makefile.manual F90=$FC F90FLAGS=-I../../src
  make -f Makefile.manual quicktest

  git clean -dfx
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --fast"
  make -f Makefile.manual quicktest

  git clean -dfx
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --separate-compilation"
  make -f Makefile.manual quicktest

  git clean -dfx
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --separate-compilation --fast"
  make -f Makefile.manual quicktest
'


##########################
# Section 9: fastGPT
##########################
time_section "🧪 Testing fastGPT" '
    if [[ "$RUNNER_OS" == "macos-latest" ]]; then
        git clone https://github.com/certik/fastGPT.git
        cd fastGPT

        git clean -dfx
        git checkout -t origin/namelist
        git checkout d3eef520c1be8e2db98a3c2189740af1ae7c3e06
        # NOTE: the release file link below would not necessarily
        # need to be updated if the commit hash above is updated
        curl -f -L -o model.dat \
            https://github.com/certik/fastGPT/releases/download/v1.0.0/model_fastgpt_124M_v1.dat
        echo "11f6f018794924986b2fdccfbe8294233bb5e8ba28d40ae971dec3adbdc81ad7  model.dat" | shasum -a 256 --check

        mkdir lf
        cd lf
        FC=$FC CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_basic_input
        ./test_more_inputs
        cd ..

        # TODO: regression as of `struct refactoring`
        # mkdir lf-goc
        # cd lf-goc
        # FC="$FC --separate-compilation --rtlib" CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
        # make VERBOSE=1
        # ln -s ../model.dat .
        # ./gpt2
        # ./test_basic_input
        # ./test_more_inputs
        # cd ..

    elif [[ "$RUNNER_OS" == "ubuntu-latest" ]]; then
        git clone https://github.com/certik/fastGPT.git
        cd fastGPT
        git checkout -t origin/lf6
        git checkout bc04dbf476b6173b0bb945ff920119ffaf4a290d
        echo $CONDA_PREFIX
        FC=$FC CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS .
        make
        ls -l ./gpt2 ./chat ./test_basic_input ./test_chat ./test_more_inputs
        file ./gpt2 ./chat ./test_basic_input ./test_chat ./test_more_inputs
        ldd ./gpt2
        ldd ./chat
        ldd ./test_basic_input
        ldd ./test_chat
        ldd ./test_more_inputs

        git clean -dfx
        git checkout -t origin/lf36run
        git checkout c915a244354df2e23b0dc613e302893b496549e2
        # NOTE: the release file link below would not necessarily
        # need to be updated if the commit hash above is updated
        curl -f -L -o model.dat \
            https://github.com/certik/fastGPT/releases/download/v1.0.0/model_fastgpt_124M_v1.dat
        echo "11f6f018794924986b2fdccfbe8294233bb5e8ba28d40ae971dec3adbdc81ad7  model.dat" | shasum -a 256 --check

        mkdir lf
        cd lf
        FC=$FC CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_more_inputs
        ./test_chat
        ctest -V

        cd ..

        mkdir lf-goc
        cd lf-goc
        FC="$FC --separate-compilation --rtlib" CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_more_inputs
        ./test_chat
        ctest -V
        cd ..

        mkdir lf-fast
        cd lf-fast
        FC="$FC --fast" CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Release ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_more_inputs
        ./test_chat
        ctest -V
        cd ..

        git checkout -t origin/namelist
        git checkout d3eef520c1be8e2db98a3c2189740af1ae7c3e06

        cd lf
        git clean -dfx
        FC=$FC CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_basic_input
        ./test_more_inputs
        cd ..

        cd lf-fast
        git clean -dfx
        FC="$FC --fast" CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Release ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_basic_input
        ./test_more_inputs

        cd ..

        rm -rf fastGPT/
    fi
'

##########################
# Section 10: stdlib
##########################
time_section "🧪 Testing stdlib" '
    git clone https://github.com/czgdp1807/stdlib.git
    cd stdlib
    export PATH="$(pwd)/../../src/bin:$PATH"

    git checkout lf-21
    git checkout 176c7a28bbc7a8a9b63441f7dfa980aeafbddd0f
    micromamba install -c conda-forge fypp

    git clean -fdx
    FC=$FC cmake . -DTEST_DRIVE_BUILD_TESTING=OFF -DBUILD_EXAMPLE=ON -DCMAKE_Fortran_COMPILER_WORKS=TRUE -DCMAKE_Fortran_FLAGS="--cpp --realloc-lhs"
    make -j8
    ctest
'

##########################
# Section 11: SNAP
##########################
time_section "🧪 Testing SNAP" '
    git clone https://github.com/certik/SNAP.git
    cd SNAP

    git checkout lf11
    git checkout 169a9216f2c922e94065a519efbb0a6c8b55149e
    cd ./src
    make -j8 FORTRAN=$FC FFLAGS="--no-array-bounds-checking" MPI=no OPENMP=no
    ./gsnap ../qasnap/sample/inp out

    make clean
    make -j8 FORTRAN=$FC FFLAGS="--separate-compilation --no-array-bounds-checking" MPI=no OPENMP=no
    ./gsnap ../qasnap/sample/inp out

    make clean
    make -j8 FORTRAN=$FC FFLAGS="--fast" MPI=no OPENMP=no
    ./gsnap ../qasnap/sample/inp out
'


##################################
# Final Summary and Cleanup
##################################
print_section "✅ All Third Party Code Tests Completed Successfully"

# Optional cleanup
# cd ../..
# rm -rf "$TMP_DIR"
