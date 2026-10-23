! gpu decline reason: LiveoutDerivedType
! A derived-type variable with an allocatable component that the loop
! passes whole to a procedure that defines it: a thread cannot keep a
! private copy of the whole value, whose allocatable component designates
! storage all of them share, and the procedure may define any part of it.
module gpu_decline_30_m
    implicit none
    type :: u
        real, allocatable :: w(:)
        integer :: k
    end type
contains
    pure subroutine set(p, i)
        type(u), intent(inout) :: p
        integer, intent(in) :: i
        p%k = i
    end subroutine
end module

program gpu_decline_30
    use gpu_decline_30_m
    implicit none
    type(u) :: p
    integer :: a(1000), i
    allocate(p%w(1000))
    p%w = 0
    p%k = 5
    do concurrent (i = 1:1000)
        call set(p, i)
        a(i) = p%k
        p%w(i) = 2 * i
    end do
    print *, a(7), p%w(7)
    if (a(7) /= 7) error stop 1
    if (p%w(7) /= 14) error stop 2
end program
