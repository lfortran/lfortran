! External procedures with an assumed-shape dummy and a saved counter,
! called directly and with a section: both calls share one saved variable.
!
! In save_46_a the dummy procedure takes its interface from an internal
! procedure whose name, save_46_g, is also that of an external procedure of
! this file with another interface. The by-data copy keeps the internal
! procedure as an interface, which must not declare the external one.
!
! In save_46_b the dummy procedure's interface is a local abstract interface
! that imports a local variable for a bound. That declaration is never
! evaluated, so the copy is still forwarded.
subroutine save_46_a(a, f, n)
implicit none
double precision, intent(in) :: a(:)
procedure(save_46_g) :: f
integer, intent(out) :: n
integer, save :: k = 0
k = k + 1
n = k + 0 * f(1)
contains
    integer function save_46_g(i)
    integer, intent(in) :: i
    save_46_g = i
    end function
end subroutine

subroutine save_46_b(x, g, n)
implicit none
double precision, intent(in) :: x(:)
integer :: k
abstract interface
    integer function iface(y)
    import :: k
    double precision, intent(in) :: y(k)
    end function
end interface
procedure(iface) :: g
integer, intent(out) :: n
integer, save :: c = 0
k = 2
c = c + 1
n = c + 0 * g(x(1:2))
end subroutine

integer function save_46_h(i)
implicit none
integer, intent(in) :: i
save_46_h = i
end function

integer function save_46_y(y)
implicit none
double precision, intent(in) :: y(*)
save_46_y = int(y(1))
end function

real function save_46_g(x, y)
implicit none
real, intent(in) :: x, y
save_46_g = x + y
end function

program save_46
implicit none
interface
    subroutine save_46_a(a, f, n)
    double precision, intent(in) :: a(:)
    interface
        integer function f(i)
        integer, intent(in) :: i
        end function
    end interface
    integer, intent(out) :: n
    end subroutine
    subroutine save_46_b(x, g, n)
    double precision, intent(in) :: x(:)
    interface
        integer function g(y)
        double precision, intent(in) :: y(*)
        end function
    end interface
    integer, intent(out) :: n
    end subroutine
    integer function save_46_h(i)
    integer, intent(in) :: i
    end function
    integer function save_46_y(y)
    double precision, intent(in) :: y(*)
    end function
    real function save_46_g(x, y)
    real, intent(in) :: x, y
    end function
end interface
double precision :: a(6)
integer :: n
a = 1
call save_46_a(a, save_46_h, n)
if (n /= 1) error stop 1
call save_46_a(a(1:6:2), save_46_h, n)
if (n /= 2) error stop 2
call save_46_a(a, save_46_h, n)
if (n /= 3) error stop 3
call save_46_b(a, save_46_y, n)
if (n /= 1) error stop 4
call save_46_b(a(1:6:2), save_46_y, n)
if (n /= 2) error stop 5
call save_46_b(a, save_46_y, n)
if (n /= 3) error stop 6
if (abs(save_46_g(1.0, 2.0) - 3.0) > 1e-6) error stop 7
print *, n
end program
