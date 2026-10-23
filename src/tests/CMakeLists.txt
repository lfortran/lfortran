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
foreach(case IN ITEMS wide-first narrow-first same-size nonalias read-prefix
        read-same zero-size repeat discard-narrow-first discard-read-prefix
        discard-read-same discard-repeat discard-read-before-write)
    string(REPLACE "-" "_" suffix "${case}")
    add_test(NAME test_gpu_cuda_api_state_alias_${suffix}
        COMMAND test_gpu_cuda_api_state alias ${case})
    set_tests_properties(test_gpu_cuda_api_state_alias_${suffix}
        PROPERTIES TIMEOUT 30)
endforeach()
