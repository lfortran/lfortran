program real128_int_to_real_02
    ! Conversion of non-negative integer(4) values to real(real128). With
    ! --fast, LLVM rewrites `1 + real(n)` for a known non-negative n into an
    ! unsigned conversion `uitofp i32 to fp128` (the __floatunsitf libcall),
    ! including values of 2**31 and above, which the runtime must provide.
    use, intrinsic :: iso_fortran_env, only: int32, real128
    implicit none
    integer(int32) :: i, n
    real(real128) :: x, y

    n = max(command_argument_count(), 0)
    x = n
    y = 1 + x
    print *, y
    if (abs(y - 1) > 1e-30_real128) error stop 1

    do i = 0, 2
        x = max(n + i, 0)
        y = x + 2147483647
        print *, y
        if (abs(y - (2147483647.0_real128 + i)) > 0.5_real128) error stop 2
    end do
    if (y /= 2147483649.0_real128) error stop 3
end program
