! Test: elemental intrinsics on zero-sized parameter arrays.
! Regression test for ICE in compiletime_broadcast_elemental_intrinsic
! when max_array_size == 0 (AssertFailed: f != nullptr).
program empty_array_04
    implicit none

    integer, parameter :: xi(0) = 1
    real, parameter :: xr(0) = 1.0

    ! Elemental type-conversion intrinsics on zero-sized arrays
    if (size(int(xi)) /= 0) error stop
    if (size(real(xi)) /= 0) error stop
    if (size(dble(xr)) /= 0) error stop

    ! Elemental math intrinsics on zero-sized arrays
    if (size(abs(xi)) /= 0) error stop
    if (size(abs(xr)) /= 0) error stop

    print *, "PASS"
end program empty_array_04
