! A host variable with the target attribute used in an internal procedure
! must be the host variable itself, not a copy: its address must match and
! writes through host pointers must not be undone after the call.
module nested_vars_17_mod
use iso_c_binding, only: c_ptr, c_loc, c_associated
implicit none
contains
subroutine host(x, y)
real(8), intent(inout), target :: x
integer, intent(in), target :: y
real(8), pointer :: px
integer, pointer :: py
logical, target :: l
integer, target :: k
integer :: i
px => x
py => y
l = .false.
k = 0
call set()
if (x /= 3.5d0) error stop 10
if (.not. l) error stop 11
if (.not. same()) error stop 12
do i = 1, 5
    call inc()
end do
if (k /= 5) error stop 13
contains
    subroutine set()
    logical, pointer :: pl
    px = 3.5d0
    pl => l
    pl = .true.
    end subroutine
    logical function same()
    same = associated(px, x) .and. associated(py, y) .and. py == 42
    end function
    subroutine inc()
    integer, pointer :: pk
    pk => k
    pk = pk + 1
    end subroutine
end subroutine
end module

program nested_vars_17
use nested_vars_17_mod
implicit none
integer, target :: j = 7
integer, pointer :: p
type(c_ptr) :: cp
real(8) :: a
integer :: b
cp = c_loc(j)
if (.not. same_loc()) error stop 1
p => j
call set()
if (j /= 8) error stop 2
if (.not. same()) error stop 3
a = 1
b = 42
call host(a, b)
if (a /= 3.5d0) error stop 4
print *, j, a
contains
    logical function same_loc()
    same_loc = c_associated(cp, c_loc(j))
    end function
    subroutine set()
    p = 8
    end subroutine
    logical function same()
    same = associated(p, j)
    end function
end program
