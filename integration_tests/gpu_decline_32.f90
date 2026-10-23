! gpu decline reason: LiveoutDerivedType
! A local of a derived type with an allocatable component and a component
! of a derived type, of which the loop defines a component of a component
! in every iteration. The kernel cannot hold such a value as a local of its
! own yet, and a thread can keep a private copy of a scalar component of
! it, but not of a component of a component.
module gpu_decline_32_m
    implicit none
    type :: v
        integer :: k
    end type
    type :: u
        real, allocatable :: w(:)
        type(v) :: inner
    end type
end module

program gpu_decline_32
    use gpu_decline_32_m
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
