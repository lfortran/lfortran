! External procedures with an assumed-shape dummy and a saved local, whose
! declarations use internal procedures and local interfaces: a dummy
! procedure declared with an internal procedure, a local derived type with a
! procedure pointer component, local abstract interfaces that import a local
! variable or a named constant.

! The dummy procedure takes its interface from an internal procedure.
! This is the fail-closed case: the signature needs the internal procedure
! `helper`, so the by-data copy is not forwarded to the original and keeps
! its own counter. The program passes only whole arrays, so this checks
! that the procedure compiles and runs.
subroutine save_34_dummy(x, g, n)
implicit none
real, intent(in) :: x(:)
procedure(helper) :: g
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
n = c + g(1)
contains
integer function helper(k)
integer, intent(in) :: k
helper = k
end function
end subroutine

! A local derived type has a procedure pointer component whose interface is
! an internal procedure.
subroutine save_34_component(x, n)
implicit none
real, intent(in) :: x(:)
integer, intent(out) :: n
type :: holder
    procedure(helper), pointer, nopass :: pp => null()
end type
type(holder) :: h
integer, save :: c = 0
c = c + 1
h%pp => helper
n = h%pp(c)
contains
integer function helper(k)
integer, intent(in) :: k
helper = 10*k
end function
end subroutine

! A local abstract interface imports a local variable.
subroutine save_34_import(x, n)
implicit none
real, intent(in) :: x(:)
integer, intent(out) :: n
integer :: k
abstract interface
    integer function iface(y)
    import :: k
    real, intent(in) :: y(k)
    end function
end interface
procedure(iface), pointer :: p
integer, save :: c = 0
k = 2
c = c + 1
p => first
n = c + p(x(1:2))
contains
integer function first(y)
real, intent(in) :: y(k)
first = int(sum(y))
end function
end subroutine

! The dummy procedure's interface is local and imports a named constant.
subroutine save_34_interface(x, g, n)
implicit none
real, intent(in) :: x(:)
integer, parameter :: m = 2
abstract interface
    integer function iface(y)
    import :: m
    real, intent(in) :: y(m)
    end function
end interface
procedure(iface) :: g
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
n = c + g(x(1:m)) + helper()
contains
integer function helper()
helper = 0
end function
end subroutine

integer function save_34_two(k)
implicit none
integer, intent(in) :: k
save_34_two = 2*k
end function

integer function save_34_sum(y)
implicit none
real, intent(in) :: y(2)
save_34_sum = int(sum(y))
end function

program save_34
implicit none
interface
    subroutine save_34_dummy(x, g, n)
    real, intent(in) :: x(:)
    interface
        integer function g(k)
        integer, intent(in) :: k
        end function
    end interface
    integer, intent(out) :: n
    end subroutine
    subroutine save_34_component(x, n)
    real, intent(in) :: x(:)
    integer, intent(out) :: n
    end subroutine
    subroutine save_34_import(x, n)
    real, intent(in) :: x(:)
    integer, intent(out) :: n
    end subroutine
    subroutine save_34_interface(x, g, n)
    real, intent(in) :: x(:)
    interface
        integer function g(y)
        real, intent(in) :: y(2)
        end function
    end interface
    integer, intent(out) :: n
    end subroutine
    integer function save_34_two(k)
    integer, intent(in) :: k
    end function
    integer function save_34_sum(y)
    real, intent(in) :: y(2)
    end function
end interface
real :: a(6)
integer :: n
a = 1

call save_34_dummy(a, save_34_two, n)
if (n /= 3) error stop
call save_34_dummy(a, save_34_two, n)
if (n /= 4) error stop

! Whole arrays and sections share the saved counter.
call save_34_component(a, n)
if (n /= 10) error stop
call save_34_component(a(1:6:2), n)
if (n /= 20) error stop
call save_34_component(a, n)
if (n /= 30) error stop

call save_34_import(a, n)
if (n /= 3) error stop
call save_34_import(a(1:6:2), n)
if (n /= 4) error stop
call save_34_import(a, n)
if (n /= 5) error stop

call save_34_interface(a, save_34_sum, n)
if (n /= 3) error stop
call save_34_interface(a(1:6:2), save_34_sum, n)
if (n /= 4) error stop
call save_34_interface(a, save_34_sum, n)
if (n /= 5) error stop
print *, "ok"
end program
