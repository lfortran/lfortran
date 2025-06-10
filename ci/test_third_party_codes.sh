#!/bin/bash
set -e  # Exit immediately on any error

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


time_section "🧪 Testing splpak" '
  git clone https://github.com/Pranavchiku/splpak.git
  cd splpak
  export PATH="$(pwd)/../src/bin:$PATH"
  micromamba install -c conda-forge fpm

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
  FC="$FC --cpp --generate-object-code" ./run_tests.sh
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
  FC="$FC --cpp -DOPEN_MPI=yes" ./build_and_run_lfortran.sh

  print_subsection "Building with optimization flags"
  FC="$FC --cpp --fast --skip-pass=dead_code_removal -DOPEN_MPI=yes" ./build_and_run_lfortran.sh

  print_subsection "Building POT3D in separate compilation mode"
  FC="$FC --cpp --generate-object-code -DOPEN_MPI=yes" ./build_and_run_lfortran.sh

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

  git checkout n-lf-9
  git checkout 649cad24b22efbbfe7ce38e9ccb95df3eac1f6ea
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
  git checkout sc-lf-4
  git checkout 6d3370630fd0a9da8bd28d88602820848b5cc3f0
  FC=$FC cmake . \
      -DTEST_DRIVE_BUILD_TESTING=OFF \
      -DBUILD_EXAMPLE=ON -DCMAKE_Fortran_COMPILER_WORKS=TRUE \
      -DCMAKE_Fortran_FLAGS="--cpp --generate-object-code --realloc-lhs --no-warnings --use-loop-variable-after-loop -I$(pwd)/src -I$(pwd)/subprojects/test-drive/"
  make -j8
  ctest

  print_success "Done with stdlib (Less Workarounds)"
  cd ..
  rm -rf stdlib
'

##########################
# Section 2: Fortran-Primes
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
  FC="$FC --generate-object-code" ./build_and_run.sh

  print_success "Done with Fortran-Primes"
  cd ..
  rm -rf fortran-primes
'

########################################
# Section 3: Numerical Methods Fortran #
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

  FC="$FC --generate-object-code" make
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

  FC="$FC --generate-object-code --std=f23" make
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

  print_success "Done with PRIMA"
  cd ..
'

##########################
# Section 6: Legacy Minpack
##########################
time_section "🧪 Testing Legacy Minpack (SciPy)" '
  git clone https://github.com/certik/minpack.git
  cd minpack
  git checkout -t origin/scipy30
  git checkout 409ba02a107f47ee835f3976952bbc64dd46de8a
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
  FC="$FC --intrinsic-mangling --generate-object-code" cmake ..
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
  FC="$FC --intrinsic-mangling --generate-object-code --std=f23" cmake ..
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
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections
  $FC ./examples/example_lmder1.f90 --legacy-array-sections

  print_subsection "Testing with separate compilation"
  git clean -dfx
  $FC ./src/minpack.f90 -c --legacy-array-sections --generate-object-code
  $FC ./examples/example_hybrd.f90 --legacy-array-sections --generate-object-code minpack.o
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections --generate-object-code minpack.o
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections --generate-object-code minpack.o
  $FC ./examples/example_lmder1.f90 --legacy-array-sections --generate-object-code minpack.o
'

time_section "🧪 Testing Modern Minpack (Result Check)" '
  git clone https://github.com/Pranavchiku/modern_minpack.git modern_minpack_02
  cd modern_minpack_02
  git checkout -t origin/w5
  git checkout fcde66ca86348eb0c4012dbdf0f4d8dba61261d8

  $FC ./src/minpack.f90 -c --legacy-array-sections
  $FC ./examples/example_hybrd.f90 --legacy-array-sections
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections
  $FC ./examples/example_lmder1.f90 --legacy-array-sections

  print_subsection "Testing with separate compilation"
  git clean -dfx
  $FC ./src/minpack.f90 -c --legacy-array-sections --generate-object-code
  $FC ./examples/example_hybrd.f90 --legacy-array-sections --generate-object-code minpack.o
  $FC ./examples/example_hybrd1.f90 --legacy-array-sections --generate-object-code minpack.o
  $FC ./examples/example_lmdif1.f90 --legacy-array-sections --generate-object-code minpack.o
  $FC ./examples/example_lmder1.f90 --legacy-array-sections --generate-object-code minpack.o
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
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --generate-object-code"
  make -f Makefile.manual quicktest

  git clean -dfx
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --generate-object-code --fast"
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

        mkdir lf-goc
        cd lf-goc
        FC="$FC --generate-object-code --rtlib" CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
        make VERBOSE=1
        ln -s ../model.dat .
        ./gpt2
        ./test_basic_input
        ./test_more_inputs
        cd ..

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
        FC="$FC --generate-object-code --rtlib" CMAKE_PREFIX_PATH=$CONDA_PREFIX cmake -DFASTGPT_BLAS=OpenBLAS -DCMAKE_BUILD_TYPE=Debug ..
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
# Section 10: FPM
##########################
time_section "🧪 Testing FPM" '
    if [[ "$RUNNER_OS" == "ubuntu-latest" ]]; then
        git clone https://github.com/czgdp1807/fpm.git
        cd fpm
        git fetch origin lfortran_build_4
        git checkout lfortran_build_4
        git checkout 910c461f04506bf87a05a8fbaf7d1b0a79f0bd48
        export PATH="$(pwd)/../../src/bin:$PATH"
        ./build.sh
    fi
'

##########################
# Section 11: stdlib
##########################
time_section "🧪 Testing stdlib" '
    git clone https://github.com/czgdp1807/stdlib.git
    cd stdlib
    export PATH="$(pwd)/../../src/bin:$PATH"

    git checkout lf20
    git checkout abb1d33d6ae02d8b62a13be7f9e51f6117c67ba4
    micromamba install -c conda-forge fypp

    git clean -fdx
    FC=$FC cmake . -DTEST_DRIVE_BUILD_TESTING=OFF -DBUILD_EXAMPLE=ON -DCMAKE_Fortran_COMPILER_WORKS=TRUE -DCMAKE_Fortran_FLAGS="--cpp --realloc-lhs"
    make -j8
    ctest
'

##########################
# Section 12: SNAP
##########################
time_section "🧪 Testing SNAP" '
    git clone https://github.com/certik/SNAP.git
    cd SNAP

    git checkout lf11
    git checkout 169a9216f2c922e94065a519efbb0a6c8b55149e
    cd ./src
    make -j8 FORTRAN=$FC FFLAGS= MPI=no OPENMP=no
    ./gsnap ../qasnap/sample/inp out

    make clean
    make -j8 FORTRAN=$FC FFLAGS="--generate-object-code" MPI=no OPENMP=no
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
