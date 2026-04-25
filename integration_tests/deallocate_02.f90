module m_deallocate_02
implicit none

type :: t
    integer :: x = 0
contains
    final :: finalize
end type

interface t
    module procedure construct
end interface

contains

pure function construct(x) result(res)
    integer, intent(in) :: x
    type(t) :: res
    res%x = x
end function

pure function f(a) result(r)
    type(t), intent(in) :: a
    integer :: r
    r = a%x
end function

pure subroutine finalize(self)
    type(t), intent(inout) :: self
    self%x = 0
end subroutine

end module

program deallocate_02
! Test: spec expression with finalizable type constructor
! Ensures insert_deallocate pass updates function dependencies
use m_deallocate_02, only: t, f
implicit none
call sub
contains
    subroutine sub
        real :: arr(f(t(3)))
        integer :: i
        do i = 1, size(arr)
            arr(i) = real(i)
        end do
        if (size(arr) /= 3) error stop
        if (arr(1) /= 1.0) error stop
        if (arr(3) /= 3.0) error stop
        print *, "PASS"
    end subroutine
end program
