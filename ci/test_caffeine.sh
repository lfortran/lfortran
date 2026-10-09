#!/usr/bin/env bash
echo "##[group] Setup"
set -ex -o pipefail

echo "CONDA_PREFIX=$CONDA_PREFIX"
reference=false
if [[ $(uname -s) == Linux ]] ; then
  case "${LFORTRAN_CI_SCOPE:-exhaustive}" in
    quick)
      reference=$(python3 ci/coarray_tests.py reference \
        --base "${LFORTRAN_COARRAY_BASE:-}" --head "${LFORTRAN_COARRAY_HEAD:-}")
      ;;
    exhaustive)
      reference=true
      ;;
    *)
      echo "ERROR: unknown LFORTRAN_CI_SCOPE: $LFORTRAN_CI_SCOPE" >&2
      exit 2
      ;;
  esac
fi
case "$reference" in
  true|false) echo "Run GFortran/OpenCoarrays reference validation: $reference" ;;
  *) echo "ERROR: invalid coarray reference selection: $reference" >&2; exit 2 ;;
esac

# Fail before dependency setup if registrations cannot be understood.
tests=$(python3 ci/coarray_tests.py list)

# Use freshly built LFortran

export PATH="$PWD/src/bin:$PATH"

which lfortran
lfortran --version

ensure_tool() {
  local package="$1" status
  shift
  if "$@"; then
    return 0
  else
    status=$?
  fi
  # Only a missing command warrants installation; do not mask broken tools.
  if [[ "$status" != 127 ]]; then
    return "$status"
  fi
  micromamba install -y -c conda-forge "$package"
  "$@"
}

ensure_tool fpm=0.12.0 fpm --version

if [[ "$reference" == true ]] ; then

(set +x
 echo "##[endgroup]"
 echo "##[group] Install OpenMPI"
)

# Probe the MPI wrapper itself, not its configured build-time compiler.
# OpenCoarrays' CMake build selects the available GFortran separately.
ensure_tool openmpi=5.0.6=hb85ec53_102 mpifort --showme:version
export PRTE_MCA_rmaps_default_mapping_policy=:oversubscribe
export OMPI_MCA_rmaps_base_oversubscribe=1

(set +x 
 echo "##[endgroup]"
 echo "##[group] Install OpenCoarrays"
)

git clone https://github.com/sourceryinstitute/OpenCoarrays.git
cd OpenCoarrays

cmake -B build \
  -DCMAKE_INSTALL_PREFIX="$PWD/inst"

cmake --build build -j2
cmake --install build

export PATH="$PWD/inst/bin:$PATH"

cd ..

which caf
caf --version

which cafrun
cafrun --version

fi # reference

(set +x 
 echo "##[endgroup]"
 echo "##[group] Install Caffeine"
)

# Clone caffeine

git clone -b main https://github.com/BerkeleyLab/caffeine.git
cd caffeine

# Release 0.8.2
git checkout 6cdf2eafb139ccb40a9a0f2a1b74750b34a9a1ac

# Toolchain setup

export FC=lfortran
export CC=clang
export CXX=clang++

echo "FC=${FC}"
echo "CC=${CC}"
echo "CXX=${CXX}"
which clang
clang --version

# Build caffeine

./install.sh --yes --prefix=$PWD/inst --verbose --enable-rpath --enable-debug

# Output Caffeine configuration information

./run-fpm.sh info

(set +x
 echo "##[endgroup]"
 echo "##[group] Caffeine unit tests"
)

# Failures here can indicate regressions compiling Caffeine's ordinary Fortran,
# independently of LFortran's coarray lowering.
# The generated wrapper selects LFortran and GASNet; keep its four-image
# unit-test setting local so integration tests retain their own image counts.
CAF_IMAGES=4 ./run-fpm.sh test --verbose

cd ..

# Make caffeine launcher available

export PATH="$PWD/caffeine/inst/bin:$PATH"

(set +x 
 echo "##[endgroup]"
 echo "##[group] Caffeine smoke test"

)

# Ensure Caffeine we just built can pass its own end-to-end smoke test
# Note this activates LFortran's coarray pass, so failures here can indicate an LFortran regression

make -C caffeine/app prif


(set +x 
 echo "##[endgroup]"
 echo "##[group] Test setup"
)

# Number of coarray images

CAF_IMAGES=${CAF_IMAGES:-2}

echo "Using CAF_IMAGES=$CAF_IMAGES"

# OpenCoarrays (caf/cafrun) does not support character arguments to co_max/co_min,
# so the gfortran cross-check is skipped for those tests. LFortran + Caffeine still
# runs them, so LFortran's own behaviour stays verified.
# coarrays_06: gfortran 15.3 + OpenCoarrays 2.10.2 fails to link __caf_get_from_remote (coindexed get)
# coarrays_21: intermittent failures on OpenCoarrays
# coarrays_27: intermittent UCX/IB failures with OpenCoarrays + OpenMPI (4 images) - gfortran only, LFortran+Caffeine passes
# coarrays_31, coarrays_32: gfortran rejects allocate(arr_coarray[*], SOURCE/MOLD=...) for array coarrays
# coarrays_34: gfortran added change team support in 16.1, but CI tests with version 13.3, so skip for now
# coarrays_39: gfortran doesn't support coshape intrinsic with version 13.3.
# coarrays_45, coarrays_46, coarrays_47: gfortran-13/OpenCoarrays lacks support for co_broadcast of PDT/extended/allocatable derived-type arrays (strided section)
# coarrays_49: gfortran ICEs on the `ptr => co_var` declaration initializer (internal compiler error in record_reference, cgraphbuild.cc:65, with 13.3); per @bonachea 16.2 still does not run this correctly
opencoarrays_unsupported="coarrays_06 coarrays_11 coarrays_13 coarrays_21 coarrays_27 coarrays_31 coarrays_32 coarrays_34 coarrays_39 coarrays_45 coarrays_46 coarrays_47 coarrays_49"

# loop over $tests
while IFS=';' read -r -u 3 testfile num_images extra_args extrafiles || [[ -n "$testfile" ]]; do

if [ -z "$num_images" ]; then
    num_images=$CAF_IMAGES
fi

(set +x
 echo "##[endgroup]"
 echo "##[group] testing: $testfile ($num_images images)"
 echo "========================================="
 echo "Running coarray test: $testfile (images: $num_images)"
 echo "========================================="
)

base=$(basename "$testfile" .f90)

# ----------------------------------------
# Compile with LFortran + caffeine
# ----------------------------------------

lfortran "$@" $extrafiles $testfile \
    $extra_args \
    -o "${base}_lf.out" \
    -L$PWD/caffeine/inst/lib \
    -lcaffeine \
    -lgasnet-smp-seq

# ----------------------------------------
# Run LFortran executable
# ----------------------------------------

gasnetrun_smp -n "$num_images" ./"${base}_lf.out"

# ----------------------------------------
# Cross-check with gfortran/OpenCoarrays, unless OpenCoarrays lacks support
# ----------------------------------------

if [[ "$reference" == true ]] ; then
  if [[ " $opencoarrays_unsupported " =~ " $base " ]] ; then
    skip_opencoarrays=true
  else
    skip_opencoarrays=false
  fi
else
  skip_opencoarrays=true
fi

if [ "$skip_opencoarrays" = true ]; then
    echo "Skipping OpenCoarrays cross-check for $testfile"
else
    caf $extrafiles $testfile -o "${base}_gf.out"
    cafrun -np "$num_images" ./"${base}_gf.out" 2>&1 \
      | sed '/Error: OSC UCX component priority/{N;/\n[[:space:]]*$/d}' # filter persistent non-fatal errors
    test ${PIPESTATUS[0]} = 0
    rm -f "${base}_gf.out"
fi

rm -f "${base}_lf.out"

echo "PASS: $testfile"

done 3<<< "$tests" # end of while loop over tests

(set +x 
 echo "##[endgroup]"
)

echo
echo "All coarray runtime tests passed"

rm -rf caffeine
if [[ "$reference" == true ]]; then
    rm -rf OpenCoarrays
fi
