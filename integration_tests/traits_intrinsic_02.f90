! Test-first extension fixture for explicit provider USE plus a facade.
program traits_intrinsic_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_intrinsic_02_facade_m
    implicit none

    integer :: i
    real(real64) :: r8

    i = 5
    r8 = 2.5_real64

    if (read_value(i) /= 15) error stop 201
    if (read_value(r8) /= 22) error stop 202
    if (facade_read_value(i) /= 16) error stop 203
    if (facade_read_value{real(real64)}(r8) /= 23) error stop 204
end program traits_intrinsic_02
