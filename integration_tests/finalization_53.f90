! A function host points a procedure pointer at one of its internal
! procedures and has a finalizable local, whose final procedure invokes the
! pointer: the local is finalized before the host gives its frame slot
! back, also when it returns early, and its result is the value it set.
module finalization_53_mod
implicit none
abstract interface
    integer function getter()
    end function
end interface
procedure(getter), pointer :: saved => null()
integer :: seen = 0
type :: t
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    if (associated(saved)) seen = saved()
end subroutine
integer function host(n) result(r)
    integer, intent(in) :: n
    type(t) :: x
    saved => get
    r = 2 * n
    if (n > 5) return
    r = 3 * n
contains
    integer function get()
        get = n + r
    end function
end function
end module

program finalization_53
use finalization_53_mod
implicit none
integer :: r
r = host(8)
print *, r, seen
if (r /= 16) error stop
if (seen /= 24) error stop
r = host(2)
print *, r, seen
if (r /= 6) error stop
if (seen /= 8) error stop
end program
