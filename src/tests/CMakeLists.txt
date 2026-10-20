add_library(doctest STATIC doctest.cpp)
add_library(p::doctest ALIAS doctest)

add_executable(test_gpu_cuda_api_state test_gpu_cuda_api_state.cpp)
target_include_directories(test_gpu_cuda_api_state PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/cuda_api_test_double")
add_test(NAME test_gpu_cuda_api_state COMMAND test_gpu_cuda_api_state)
add_test(NAME test_gpu_cuda_api_state_read_only
    COMMAND test_gpu_cuda_api_state read-only)
add_test(NAME test_gpu_cuda_api_state_resident
    COMMAND test_gpu_cuda_api_state resident)
