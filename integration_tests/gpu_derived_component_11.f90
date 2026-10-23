! Two derived-type variables whose names and component names spell the
! same chain, `y%inner%k` and `y_inner%k`, of which one iteration of a `do
! concurrent` each defines that scalar component: each thread keeps a
! private copy of each, under names of their own.
module gpu_derived_component_11_m
    implicit none
    type :: v
        integer :: k
    end type
    type :: u
        real, allocatable :: w(:)
        type(v) :: inner
    end type
    type :: t
        real, allocatable :: w(:)
        integer :: k
    end type
end module

program gpu_derived_component_11
    use gpu_derived_component_11_m
    implicit none
    type(u) :: y
    type(t) :: y_inner
    integer :: i
    allocate(y%w(10))
    allocate(y_inner%w(10))
    y%w = 0
    y_inner%w = 0
    y%inner%k = 0
    y_inner%k = 0
    do concurrent (i = 1:10)
        y%w(i) = i
        y_inner%w(i) = 2 * i
        if (i == 1) y_inner%k = 11
        if (i == 2) y%inner%k = 22
    end do
    print *, y%inner%k, y_inner%k, y%w(10), y_inner%w(10)
    if (y%inner%k /= 22) error stop 1
    if (y_inner%k /= 11) error stop 2
    if (y%w(10) /= 10) error stop 3
    if (y_inner%w(10) /= 20) error stop 4
end program
