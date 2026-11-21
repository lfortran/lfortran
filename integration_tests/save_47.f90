! External procedures with an assumed-shape dummy and a saved counter whose
! dummy's bound inquires, with a DIM known only at run time, about a saved
! character array (save_47_s) or about the length of a substring of an
! element of a DATA-initialized one (save_47_d), called directly, through a dummy
! procedure, through a procedure pointer and with a section: all calls
! share one saved variable. The by-data copy forwards to the original and
! keeps the array only for the inquiry, which must not allocate it on every
! call without freeing it (checked with --detect-leaks).
subroutine save_47_s(a, n, x, info)
implicit none
integer, intent(in) :: n
real, intent(in) :: a(:)
character(len=5), save :: c(3, 4)
real, intent(inout) :: x(size(c, n - 3))
integer, intent(out) :: info(3)
integer, save :: k = 0
k = k + 1
info = [k, size(a), size(x)]
end subroutine

subroutine save_47_d(a, n, x, info)
implicit none
integer, intent(in) :: n
real, intent(in) :: a(:)
character(len=5) :: c(3, 4)
data c /12*"ab"/
real, intent(inout) :: x(len(c(n, 1)(n:5)))
integer, intent(out) :: info(3)
integer, save :: k = 0
k = k + 1
info = [k, size(a), size(x)]
end subroutine

program save_47
implicit none
interface
    subroutine save_47_s(a, n, x, info)
    integer, intent(in) :: n
    real, intent(in) :: a(:)
    real, intent(inout) :: x(*)
    integer, intent(out) :: info(3)
    end subroutine
    subroutine save_47_d(a, n, x, info)
    integer, intent(in) :: n
    real, intent(in) :: a(:)
    real, intent(inout) :: x(*)
    integer, intent(out) :: info(3)
    end subroutine
end interface
procedure(save_47_s), pointer :: p
real :: a(3), x(100)
integer :: info(3)
a = 1
call save_47_s(a, 4, x, info)
if (any(info /= [1, 3, 3])) error stop 1
call run_s(save_47_s)
if (any(info /= [2, 3, 3])) error stop 2
p => save_47_s
call p(a, 4, x, info)
if (any(info /= [3, 3, 3])) error stop 3
call save_47_s(a(1:3:2), 4, x, info)
if (any(info /= [4, 2, 3])) error stop 4
call save_47_d(a, 2, x, info)
if (any(info /= [1, 3, 4])) error stop 5
call run_d(save_47_d)
if (any(info /= [2, 3, 4])) error stop 6
p => save_47_d
call p(a, 2, x, info)
if (any(info /= [3, 3, 4])) error stop 7
call save_47_d(a(1:3:2), 2, x, info)
if (any(info /= [4, 2, 4])) error stop 8
print *, info
contains
    subroutine run_s(f)
    procedure(save_47_s) :: f
    call f(a, 4, x, info)
    end subroutine
    subroutine run_d(f)
    procedure(save_47_d) :: f
    call f(a, 2, x, info)
    end subroutine
end program
