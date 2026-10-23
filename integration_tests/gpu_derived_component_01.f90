! A scalar component of a derived-type variable the host reads after a
! `do concurrent` in which only one iteration defines it keeps that value
! (Fortran 2018, 11.1.7.5).
program gpu_derived_component_01
    implicit none
    type :: u
        real, allocatable :: w(:)
        integer :: k
    end type
    type :: v
        integer :: k
        real :: x
    end type
    type(u) :: y
    type(v) :: z
    integer :: i
    allocate(y%w(3))
    y%w = 1
    y%k = 5
    do concurrent (i = 1:3)
        if (i == 1) y%k = 7
    end do
    print *, y%k
    if (y%k /= 7) error stop 1
    if (any(y%w /= 1)) error stop 2
    z%k = 5
    z%x = 1.5
    do concurrent (i = 1:3)
        if (i == 2) z%k = 10 * i
    end do
    print *, z%k, z%x
    if (z%k /= 20) error stop 3
    if (z%x /= 1.5) error stop 4
end program
