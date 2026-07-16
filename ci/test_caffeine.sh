#!/usr/bin/env bash
echo "##[group] Setup"
set -ex

echo "CONDA_PREFIX=$CONDA_PREFIX"
if [[ $(uname -s) == Linux ]] ; then
  LINUX=1
fi

# Use freshly built LFortran

export PATH="$PWD/src/bin:$PATH"

which lfortran
lfortran --version

micromamba install -c conda-forge fpm=0.12.0

which fpm
fpm --version

if [ $LINUX ] ; then

(set +x
 echo "##[endgroup]"
 echo "##[group] Install OpenMPI"
)

micromamba install -y -c conda-forge openmpi
export PRTE_MCA_rmaps_default_mapping_policy=:oversubscribe
export OMPI_MCA_rmaps_base_oversubscribe=1

(set +x 
 echo "##[endgroup]"
 echo "##[group] Install OpenCoarrays"
)

git clone https://github.com/sourceryinstitute/OpenCoarrays.git
cd OpenCoarrays

cmake -B build \
  -DCMAKE_INSTALL_PREFIX="$HOME/opencoarrays"

cmake --build build -j2
cmake --install build

export PATH="$HOME/opencoarrays/bin:$PATH"

cd ..

which caf
caf --version

which cafrun
cafrun --version

fi # LINUX

(set +x 
 echo "##[endgroup]"
 echo "##[group] Install Caffeine"
)

# Clone caffeine

git clone -b main https://github.com/BerkeleyLab/caffeine.git
cd caffeine

# Release 0.8.0
git checkout 9a4a818d9617bc88890a9fdc9fd6e66959c7fad0

# Toolchain setup

export FC=lfortran
export CC=clang
export CXX=clang++

echo "FC=${FC}"
echo "CC=${CC}"
echo "CXX=${CXX}"
which clang
clang --version

# inject ISO_Fortran_binding.h into the C include path
export CPPFLAGS="-I$(lfortran --print-c-include-dir)"

# GASNet debug options

export GASNET_CONFIGURE_ARGS="--enable-rpath --enable-debug"

# Build caffeine

./install.sh --yes --prefix=$PWD/inst --verbose

# Output Caffeine configuration information

./run-fpm.sh info

cd ..

# Make caffeine launcher available

export PATH="$PWD/caffeine/inst/bin:$PATH"

(set +x 
 echo "##[endgroup]"
 echo "##[group] Test setup"
)

# Number of coarray images

CAF_IMAGES=${CAF_IMAGES:-2}

echo "Using CAF_IMAGES=$CAF_IMAGES"

# Find all coarray-enabled tests

tests=$(python3 -c '
import re
filenames = []

with open("integration_tests/CMakeLists.txt") as f:
    for line in f:
        line = line.strip()
        if line.startswith("RUN(") and "coarray=true" in line:
            m = re.search(r"NAME\s+(\w+)", line)
            if m:
                num_images = ""
                m_img = re.search(r"NUM_IMAGES[\s=]+(\d+)", line)
                if m_img:
                    num_images = m_img.group(1)
                filenames.append(f"integration_tests/{m.group(1)}.f90:{num_images}")

print(" ".join(filenames))
')

if [ -z "$tests" ]; then
echo "No coarray tests found"
exit 1
fi

# OpenCoarrays (caf/cafrun) does not support character arguments to co_max/co_min,
# so the gfortran cross-check is skipped for those tests. LFortran + Caffeine still
# runs them, so LFortran's own behaviour stays verified.
# coarrays_21: intermittent failures on OpenCoarrays
opencoarrays_unsupported="coarrays_11 coarrays_13 coarrays_21"

for test_info in $tests; do
testfile="${test_info%%:*}"
num_images="${test_info##*:}"

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

lfortran "$testfile" \
    --coarray=true \
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

if [ $LINUX ] ; then
  skip_opencoarrays=false
  for skip in $opencoarrays_unsupported; do
      if [ "$base" = "$skip" ]; then
          skip_opencoarrays=true
      fi
  done
else # macOS
  skip_opencoarrays=true
fi

if [ "$skip_opencoarrays" = true ]; then
    echo "Skipping OpenCoarrays cross-check for $testfile"
else
    caf "$testfile" -o "${base}_gf.out"
    cafrun -np "$num_images" ./"${base}_gf.out" 2>&1 \
      | sed '/Error: OSC UCX component priority/{N;/\n[[:space:]]*$/d}' # filter persistent non-fatal errors
    rm -f "${base}_gf.out"
fi

rm -f "${base}_lf.out"

echo "PASS: $testfile"

done

(set +x 
 echo "##[endgroup]"
)

echo
echo "All coarray runtime tests passed"

rm -rf caffeine
rm -rf OpenCoarrays
