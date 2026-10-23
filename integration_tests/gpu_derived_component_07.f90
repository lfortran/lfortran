! A derived-type variable with an allocatable component of which one
! iteration of a `do concurrent` defines a component of a component, and
! every iteration an element of the allocatable component: each thread
! keeps a private copy of the scalar the iteration defines, and the host
! sees both after the loop.
module gpu_derived_component_07_m
    implicit none
    type :: v
        integer :: k
    end type
    type :: u
        real, allocatable :: w(:)
        type(v) :: inner
    end type
end module

program gpu_derived_component_07
    use gpu_derived_component_07_m
    implicit none
    type(u) :: y
    integer :: i
    allocate(y%w(100))
    y%w = 0
    y%inner%k = 0
    do concurrent (i = 1:100)
        y%w(i) = 2 * i
        if (i == 3) y%inner%k = 7
    end do
    print *, y%w(5), y%inner%k
    if (y%w(5) /= 10) error stop 1
    if (y%inner%k /= 7) error stop 2
end program
