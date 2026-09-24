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

# Cherry-pick a recent fix to -DCAF_IMPORT_TEAM_CONSTANTS
git config user.email "nobody@nowhere.com"
git config user.name  "Nobody"
git cherry-pick 736130c4af77b4ab33e4341e6dcd32ab4c8b7f4a

# Cherry-pick recent fixes to assertion reporting for LFortran (Caffeine PR #353)
git cherry-pick 4ccb611328908c9fdee05d0bab587baa4ac679db
# Sadly git-merge lacks the ability to ignore irrelevant changes
# on adjacenet lines, so this critical one-line commit doesn't apply cleanly:
#git cherry-pick 34652e1e215ab08eabac2642b6db82c9beac944f
# Apply it manually instead:
sed -i.bak '\|assert\.git|s/3\.1\.0/3.1.2/' manifest/fpm.toml.template

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

# instruct Caffeine to import the iso_fortran_env constants from LFortran
CPPFLAGS+=" -DCAF_IMPORT_CONSTANTS"

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
            fields = ["NAME", "NUM_IMAGES", "LABELS", "EXTRAFILES", "EXTRA_ARGS"]
            # Regex pattern matching key, separator (space or =), and value up to the next key or closing bracket
            fields_pattern = "|".join(fields)
            pattern = rf"({fields_pattern})[ =]\s*(.*?)(?=\s+(?:{fields_pattern})[ =]|\))"
            parsed_data = dict(re.findall(pattern, line))
            name       = parsed_data.get("NAME")
            num_images = parsed_data.get("NUM_IMAGES") or ""
            extra_args = parsed_data.get("EXTRA_ARGS") or ""
            extrafiles = parsed_data.get("EXTRAFILES") or ""
            extrafiles = " ".join(f"integration_tests/{item}" for item in extrafiles.split())
            if name:
                filenames.append(f"integration_tests/{name}.f90;{num_images};{extra_args};{extrafiles}")

print("\n".join(filenames))
')

if [ -z "$tests" ]; then
echo "No coarray tests found"
exit 1
fi

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

lfortran $extrafiles $testfile \
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

if [ $LINUX ] ; then
  if [[ " $opencoarrays_unsupported " =~ " $base " ]] ; then
    skip_opencoarrays=true
  else
    skip_opencoarrays=false
  fi
else # macOS
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
rm -rf OpenCoarrays
