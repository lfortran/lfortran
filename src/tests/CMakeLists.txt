add_library(doctest STATIC doctest.cpp)
add_library(p::doctest ALIAS doctest)

# The production CUDA runtime on the CPU emulation of the CUDA runtime API,
# whose cuda_runtime.h the include path has to find.
add_executable(test_gpu_cuda_api_state test_gpu_cuda_api_state.cpp
    ../libasr/runtime/cuda_cpu/cuda_cpu_runtime.c)
target_include_directories(test_gpu_cuda_api_state PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/../libasr/runtime/cuda_cpu")
add_test(NAME test_gpu_cuda_api_state COMMAND test_gpu_cuda_api_state)
add_test(NAME test_gpu_cuda_api_state_read_only
    COMMAND test_gpu_cuda_api_state read-only)
add_test(NAME test_gpu_cuda_api_state_small_reads
    COMMAND test_gpu_cuda_api_state small-reads)
add_test(NAME test_gpu_cuda_api_state_resident
    COMMAND test_gpu_cuda_api_state resident)
add_test(NAME test_gpu_cuda_api_state_discard
    COMMAND test_gpu_cuda_api_state discard)
add_test(NAME test_gpu_cuda_api_state_discard_read_only
    COMMAND test_gpu_cuda_api_state discard read-only)
add_test(NAME test_gpu_cuda_api_state_discard_resident
    COMMAND test_gpu_cuda_api_state discard resident)
add_test(NAME test_gpu_cuda_api_state_discard_fill
    COMMAND test_gpu_cuda_api_state discard-fill)
add_test(NAME test_gpu_cuda_api_state_read_once
    COMMAND test_gpu_cuda_api_state read-once)
# These count the copies of buffers of a few hundred bytes at every launch,
# which the cache of small reads would not make.
set_tests_properties(test_gpu_cuda_api_state_read_only
    test_gpu_cuda_api_state_discard_read_only
    test_gpu_cuda_api_state_read_once PROPERTIES
    ENVIRONMENT "LFORTRAN_GPU_SMALL_READS=0")
add_test(NAME test_gpu_cuda_api_state_keep_reads
    COMMAND test_gpu_cuda_api_state keep-reads)
add_test(NAME test_gpu_cuda_api_state_keep_across_waits
    COMMAND test_gpu_cuda_api_state keep-across-waits)
add_test(NAME test_gpu_cuda_api_state_idle_sync_pool
    COMMAND test_gpu_cuda_api_state idle-sync-pool)
add_test(NAME test_gpu_cuda_api_state_failed_free
    COMMAND test_gpu_cuda_api_state failed-free)
set_tests_properties(test_gpu_cuda_api_state_failed_free PROPERTIES
    PASS_REGULAR_EXPRESSION "lfortran_gpu: cudaFree failed: invalid argument")
add_test(NAME test_gpu_cuda_api_state_unified
    COMMAND test_gpu_cuda_api_state unified)
set_tests_properties(test_gpu_cuda_api_state_unified PROPERTIES
    ENVIRONMENT "LFORTRAN_GPU_MEMORY=unified")
add_test(NAME test_gpu_cuda_api_state_managed
    COMMAND test_gpu_cuda_api_state managed)
set_tests_properties(test_gpu_cuda_api_state_managed PROPERTIES
    ENVIRONMENT "LFORTRAN_GPU_MEMORY=managed"
    PASS_REGULAR_EXPRESSION "passed on the CUDA runtime API 13000")
add_test(NAME test_gpu_cuda_api_state_managed_pool
    COMMAND test_gpu_cuda_api_state managed-pool)
set_tests_properties(test_gpu_cuda_api_state_managed_pool PROPERTIES
    ENVIRONMENT "LFORTRAN_GPU_MEMORY=managed"
    PASS_REGULAR_EXPRESSION "managed pool capacity passed")
# The emulation provides the CUDA 13 runtime API by default; the runtime has
# to compile against, and run on, the CUDA 12 one too.
add_executable(test_gpu_cuda_api_state_cuda12 test_gpu_cuda_api_state.cpp
    ../libasr/runtime/cuda_cpu/cuda_cpu_runtime.c)
target_include_directories(test_gpu_cuda_api_state_cuda12 PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/../libasr/runtime/cuda_cpu")
target_compile_definitions(test_gpu_cuda_api_state_cuda12 PRIVATE
    CUDART_VERSION=12080)
add_test(NAME test_gpu_cuda_api_state_cuda12_managed
    COMMAND test_gpu_cuda_api_state_cuda12 managed)
set_tests_properties(test_gpu_cuda_api_state_cuda12_managed PROPERTIES
    ENVIRONMENT "LFORTRAN_GPU_MEMORY=managed"
    PASS_REGULAR_EXPRESSION "passed on the CUDA runtime API 12080")
add_test(NAME test_gpu_cuda_api_state_cuda12
    COMMAND test_gpu_cuda_api_state_cuda12)
add_test(NAME test_gpu_cuda_api_state_no_device
    COMMAND test_gpu_cuda_api_state no-device)
set_tests_properties(test_gpu_cuda_api_state_no_device PROPERTIES
    PASS_REGULAR_EXPRESSION
        "lfortran_gpu: cudaSetDevice failed: no CUDA-capable device is detected"
    FAIL_REGULAR_EXPRESSION "returned without a device")
foreach(case IN ITEMS wide-first narrow-first same-size nonalias read-prefix
        read-same zero-size repeat discard-narrow-first discard-read-prefix
        discard-read-same discard-repeat discard-read-before-write)
    string(REPLACE "-" "_" suffix "${case}")
    add_test(NAME test_gpu_cuda_api_state_alias_${suffix}
        COMMAND test_gpu_cuda_api_state alias ${case})
    set_tests_properties(test_gpu_cuda_api_state_alias_${suffix}
        PROPERTIES TIMEOUT 30)
endforeach()
