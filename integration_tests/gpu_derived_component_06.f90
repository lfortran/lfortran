! A local of a derived type with an allocatable component and a component
! of a derived type, whose scalar component every iteration of a `do
! concurrent` defines and then reads, is a temporary of the iteration
! (Fortran 2018, 11.1.7.5), although its value is not read after the loop.
module gpu_derived_component_06_m
    implicit none
    type :: v
        integer :: k
    end type
    type :: u
        real, allocatable :: w(:)
        type(v) :: inner
        integer :: k
    end type
end module

program gpu_derived_component_06
    use gpu_derived_component_06_m
    implicit none
    integer, parameter :: n = 100000
    type(u) :: y
    integer, allocatable :: a(:)
    integer :: i
    allocate(a(n), y%w(n))
    y%w = 0
    do concurrent (i = 1:n)
        y%k = 2 * i
        a(i) = y%k - i
        y%w(i) = i
    end do
    do i = 1, n
        if (a(i) /= i) error stop 1
    end do
    print *, a(1), a(n)
end program
