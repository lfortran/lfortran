#!/bin/bash

# Setup
set -e
no_of_threads=8 # default no of threads is 8
avail_backends="llvm cpp x86 wasm"
base_dir="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"; # get base_dir, taken from StackOverFlow
export PATH="$base_dir/../src/bin:$PATH"
# delete previously created directories (if any)
for back in $avail_backends; do rm -rf "$base_dir/test-$back"; done;

# Helper function used by print_help() to draw box around the help message
box_out () {
  local s=("$@") b w
  for l in "${s[@]}"; do ((w<${#l})) && { b="$l"; w="${#l}"; }; done;
  tput setaf 3
  echo " -${b//?/-}-
| ${b//?/ } |"
  for l in "${s[@]}"; do printf '| %s%*s%s |\n' "$(tput setaf 4)" "-$w" "$l" "$(tput setaf 3)"; done;
  echo "| ${b//?/ } |
 -${b//?/-}-"
  tput sgr 0
}

print_help() {
    local msg1="Usage: run_tests.sh [-h] [-j <no_of_threads> | -n <no_of_threads>] [-t <backend>]"
    local msg2="Example: run_tests.sh -n 16 -t llvm -t wasm"
    box_out "$msg1" "$msg2";
}

is_backend_supported () {
    # In bash, 0 is true and 1 is false
    for back in $avail_backends; do [ "$1" == "$back" ] && return 0; done;
    return 1;
}

run_test () {
    mkdir "$base_dir/test-$1"
    cd "$base_dir/test-$1"
    FC=lfortran cmake "-DLFORTRAN_BACKEND=$1" "-DCURRENT_BINARY_DIR=$base_dir/test-$1" -S "$base_dir" -B "$base_dir/test-$1"
    make "-j$no_of_threads"
    ctest "-j$no_of_threads" --output-on-failure
}

test_backend () {
    if is_backend_supported $1; then
        printf "Testing For Backend: $1\n";
    else
        printf "Unsupported Backend: $1\n";
        return;
    fi

    run_test "$1"
}

main () {
    while getopts ":j:n:t:h" opt; do
    case ${opt} in
        t ) test_backend "$OPTARG";;
        h ) print_help;;
        j ) no_of_threads="$OPTARG";;
        n ) no_of_threads="$OPTARG";;
        \?)
        printf "Invalid Option: -$pt $OPTARG\n";
        print_help;
        exit 1;;
    esac
    done
    shift $((OPTIND -1)) # this shifts the values for positional arugments
}

if [ $# -eq 0 ]; then
    printf "Error: No Arguments Passed.\n";
    print_help;
fi

main "$@" # pass all script arugments to main function
