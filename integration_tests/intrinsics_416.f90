! Test for https://github.com/lfortran/lfortran/issues/6124
! Subnormal floating point values (epsilon*tiny, nearest, transfer)
program intrinsics_416
    implicit none
    integer, parameter :: i4 = kind(1), i8 = selected_int_kind(15)
    real :: r32_subnorm, r32_nearest, r32_transfer
    real(8) :: r64_subnorm, r64_nearest, r64_transfer

    r32_subnorm = epsilon(1.0) * tiny(1.0)
    r32_nearest = nearest(0.0, 1.0)
    r32_transfer = transfer(1_i4, 1.0)

    r64_subnorm = epsilon(1d0) * tiny(1d0)
    r64_nearest = nearest(0d0, 1d0)
    r64_transfer = transfer(1_i8, 1d0)

    print "(ES15.7)", r32_subnorm, r32_nearest, r32_transfer
    print "(ES24.16)", r64_subnorm, r64_nearest, r64_transfer

    if (r32_subnorm /= r32_nearest) error stop
    if (r32_subnorm /= r32_transfer) error stop
    if (r64_subnorm /= r64_nearest) error stop
    if (r64_subnorm /= r64_transfer) error stop
end program
