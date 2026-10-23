! gpu decline reason: LiveoutDerivedType
! A derived-type variable with an allocatable component of which the loop
! defines a component of a component: a thread can keep a private copy of
! a scalar component of the variable, but not of the whole value, whose
! allocatable component designates storage all of them share.
module gpu_decline_31_m
    implicit none
    type :: v
        integer :: k
    end type
    type :: u
        real, allocatable :: w(:)
        type(v) :: inner
    end type
end module

program gpu_decline_31
    use gpu_decline_31_m
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
