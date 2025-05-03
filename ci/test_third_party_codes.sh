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


##########################
# Section 1: stdlib (Less Workarounds)
##########################
time_section "🧪 Testing stdlib (Less Workarounds)" '
  git clone https://github.com/czgdp1807/stdlib.git
  cd stdlib
  export PATH="$(pwd)/../src/bin:$PATH"

  git checkout n-lf-5
  git checkout 00a68c42ecf731c5b7b3d656203d6c20caae0dfa
  micromamba install -c conda-forge fypp

  git clean -fdx
  FC=$FC cmake . -DTEST_DRIVE_BUILD_TESTING=OFF -DBUILD_EXAMPLE=ON -DCMAKE_Fortran_COMPILER_WORKS=TRUE -DCMAKE_Fortran_FLAGS="--cpp --realloc-lhs --no-warnings -I$(pwd)/src -I$(pwd)/subprojects/test-drive/"
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


  print_success "Done with Numerical Methods Fortran"

  cd ..
'

######################
# Section 4: POT3D    #
######################
time_section "🧪 Testing POT3D" '
  git clone https://github.com/gxyd/pot3d.git
  cd pot3d
  git checkout -t origin/lf_hdf5_mpi_namelist_global_workarounds
  git checkout 83e1e90db7e7517fcbe8c7bce5ba309addfb23f6

  print_subsection "Building with default flags"
  FC=$FC ./build_and_run.sh

  print_subsection "Building with optimization flags"
  FC="$FC --fast --skip-pass=dead_code_removal" ./build_and_run.sh

  print_success "Done with POT3D"
  cd ..
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

  print_subsection "Testing with separate compilation"
  cd ../
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
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --generate-object-code --skip-pass=pass_array_by_data"
  make -f Makefile.manual quicktest

  git clean -dfx
  make -f Makefile.manual F90=$FC F90FLAGS="-I../../src --generate-object-code --skip-pass=pass_array_by_data --fast"
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
        curl -f -L -o model.dat \
                https://github.com/gxyd/gpt/releases/download/v2.0/model_fastgpt_124M_v1.dat
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
        curl -f -L -o model.dat \
                https://github.com/gxyd/gpt/releases/download/v2.0/model_fastgpt_124M_v1.dat
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
