! A local of a derived type with an allocatable component and a component
! of a derived type, of which every iteration of a `do concurrent` defines
! and then reads a component of a component: a temporary of the iteration
! (Fortran 2018, 11.1.7.5), which each thread keeps a private copy of.
module gpu_derived_component_08_m
    implicit none
    type :: v
        integer :: k
    end type
    type :: u
        real, allocatable :: w(:)
        type(v) :: inner
    end type
end module

program gpu_derived_component_08
    use gpu_derived_component_08_m
    implicit none
    type(u) :: y
    integer :: i, a(100)
    allocate(y%w(100))
    do concurrent (i = 1:100)
        y%inner%k = i
        a(i) = y%inner%k
    end do
    if (a(100) /= 100) error stop 1
    print *, a(100)
end program
