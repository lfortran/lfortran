! An internal procedure reaches the storage of its host variables: the address
! of a target (#14363), a write through a host pointer (#14363), and
! equivalenced variables (#14526)
program host_assoc_05
use iso_c_binding, only: c_ptr, c_loc, c_associated
implicit none
integer, target :: j = 7
integer, pointer :: p
type(c_ptr) :: cp
integer :: i, k
equivalence (i, k)
cp = c_loc(j)
if (.not. same_address()) error stop 1
p => j
call set()
if (j /= 8) error stop 2
if (.not. same_target()) error stop 3
i = 5
if (k /= 5) error stop 4
call set_k()
if (i /= 11) error stop 5
call set_i()
if (k /= 13) error stop 6
call target_host()
print *, "ok"
contains
    logical function same_address()
        same_address = c_associated(cp, c_loc(j))
    end function
    subroutine set()
        p = 8
    end subroutine
    logical function same_target()
        same_target = associated(p, j)
    end function
    subroutine set_k()
        k = 11
        if (i /= 11) error stop 7
    end subroutine
    subroutine set_i()
        i = 13
        if (k /= 13) error stop 8
    end subroutine
end program host_assoc_05

subroutine target_host()
use iso_c_binding, only: c_ptr, c_loc, c_associated
implicit none
real, target :: x(3)
type(c_ptr) :: cx
x = 1.0
cx = c_loc(x)
if (.not. same()) error stop 9
contains
    logical function same()
        same = c_associated(cx, c_loc(x))
    end function
end subroutine target_host
