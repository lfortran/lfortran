program implied_do_loops46
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none

    integer :: i
    integer, parameter :: values(*) = [(int(1.5_real128 + i), i = 1, 2)]

    if (size(values) /= 2) error stop 1
    if (values(1) /= 2) error stop 2
    if (values(2) /= 3) error stop 3
end program implied_do_loops46
