program real128_cast_01
    use real128_cast_01_mod, only: scale_half
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    ! Binary128 1.0. transfer() yields the low half first on little-endian.
    integer(8), parameter :: one_low = 0_8
    integer(8), parameter :: one_high = 4611404543450677248_8
    real(real128) :: x
    integer(8) :: bits(2)
    call scale_half(2.0_real128, x)
    print *, x
    bits = transfer(x, bits)
    if (bits(1) /= one_low) error stop 1
    if (bits(2) /= one_high) error stop 2
end program real128_cast_01
