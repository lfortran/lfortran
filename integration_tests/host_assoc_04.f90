! An internal procedure that uses a host variable of a type with final
! procedures does not finalize it: a class(T), allocatable variable of the
! main program (#15564) and an allocatable array of a procedure (#14753)
module host_assoc_04_m
implicit none
integer :: finals = 0
integer :: nfinal = 0

type :: base
    integer :: n = 0
contains
    final :: finish
end type

type :: a_t
    integer :: x = 0
contains
    final :: fin
    final :: fin1
end type

contains

subroutine finish(self)
    type(base), intent(inout) :: self
    finals = finals + 1
end subroutine

subroutine fin(self)
    type(a_t), intent(inout) :: self
    nfinal = nfinal + 1
end subroutine

subroutine fin1(self)
    type(a_t), intent(inout) :: self(:)
    nfinal = nfinal + 1
end subroutine

subroutine s2()
    type(a_t), allocatable :: t(:)
    allocate(t(2))
    t(1)%x = 5
    if (f() /= 5) error stop 1
    if (f() /= 5) error stop 2
    if (nfinal /= 0) error stop 3
contains
    integer function f()
        f = t(1)%x
    end function
end subroutine

end module host_assoc_04_m

program host_assoc_04
use host_assoc_04_m
implicit none
class(base), allocatable :: o
allocate(o)
call bump()
if (finals /= 0 .or. o%n /= 1) error stop 4
call bump()
if (finals /= 0 .or. o%n /= 2) error stop 5
call s2()
if (nfinal /= 1) error stop 6
print *, "ok"
contains
subroutine bump()
    o%n = o%n + 1
end subroutine
end program host_assoc_04
