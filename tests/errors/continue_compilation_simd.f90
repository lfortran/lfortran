program continue_compilation_simd
    implicit none
    ! This file is for testing SIMD error checks in 
    !LF$ attributes simd :: simd_2d, simd_alloc, simd_ptr, simd_scalar, simd_char, x
    real :: simd_2d(8, 8)
    real, allocatable :: simd_alloc(:)
    real, pointer :: simd_ptr(:)
    real :: simd_scalar
    character :: simd_char(8)
    real :: x

    !simd_2d
    print *, simd_2d(1,1)
    !simd_alloc
    print *, simd_alloc
    !simd_ptr
    print *, simd_ptr
    !simd_scalar
    print *, simd_scalar
    !simd_char
    print *, simd_char
    !simd_array
    print *, x

end program continue_compilation_simd