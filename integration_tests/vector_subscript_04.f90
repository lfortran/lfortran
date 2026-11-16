! An allocatable array with a vector subscript on both sides of an
! assignment: x(v) is a section, not reallocated.
program vector_subscript_04
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer, allocatable :: x(:), v(:), y(:)
    integer(int64), allocatable :: z(:), w(:)
    x = [1, 2, 3, 4]
    v = [1, 3]
    y = [5, 6]
    x(v) = x(v) + y
    print *, x
    if (any(x /= [6, 2, 9, 4])) error stop
    z = [10_int64, 20_int64, 30_int64, 40_int64]
    w = [1_int64, 2_int64]
    z(v) = z(v) + w
    print *, z
    if (any(z /= [11_int64, 20_int64, 32_int64, 40_int64])) error stop
end program
