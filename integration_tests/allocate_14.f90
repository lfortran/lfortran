module mod

    implicit none
    type t_0
        integer :: key
    end type t_0

    type :: t_1
        class(t_0), allocatable :: val
    end type t_1

    type t_2
        type(t_1), allocatable :: lst(:)
    contains
        procedure :: fill
    end type t_2
contains
    subroutine fill(self)
    class(t_2), intent(inout), target :: self
    allocate(self%lst(1))
    allocate(self%lst(1)%val)
    ! self%lst(1)%val%key = 1
    end subroutine fill
end module mod

program prog
use mod
implicit none

type(t_2) :: t
call t%fill
! if (allocated(t%lst)) then
!     print *, t%lst(1)%val%key
! end if

end program prog
