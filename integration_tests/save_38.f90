! An external procedure with an assumed-shape dummy and a saved local,
! whose dummy procedure takes its interface from an internal procedure
! whose signature refers to a dummy of the host (the bound `m`), called
! directly, through a dummy procedure and through a procedure pointer: all
! calls share one saved variable. As in much legacy code, the interface body
! declares the dummy procedure's array as assumed-size.
subroutine save_37_cnt(a, m, f, n)
double precision, intent(in) :: a(:)
integer, intent(in) :: m
procedure(g) :: f
integer, intent(out) :: n
integer, save :: k = 0
k = k + 1
n = k + 0 * f(a(1:m))
contains
    integer function g(x)
    double precision, intent(in) :: x(m)
    g = int(x(1))
    end function
end subroutine

integer function save_37_h(x)
double precision, intent(in) :: x(*)
save_37_h = int(x(1))
end function

program save_38
implicit none
interface
    subroutine save_37_cnt(a, m, f, n)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: m
    interface
        integer function f(x)
        double precision, intent(in) :: x(*)
        end function
    end interface
    integer, intent(out) :: n
    end subroutine
    integer function save_37_h(x)
    double precision, intent(in) :: x(*)
    end function
end interface
procedure(save_37_cnt), pointer :: p
double precision :: a(3)
integer :: n
a = 1
call save_37_cnt(a, 2, save_37_h, n)
if (n /= 1) error stop 1
call run(save_37_cnt, n)
if (n /= 2) error stop 2
p => save_37_cnt
call p(a, 2, save_37_h, n)
if (n /= 3) error stop 3
call save_37_cnt(a, 2, save_37_h, n)
if (n /= 4) error stop 4
print *, n
contains
    subroutine run(f, n)
    procedure(save_37_cnt) :: f
    integer, intent(out) :: n
    double precision :: b(2)
    b = 2
    call f(b, 2, save_37_h, n)
    end subroutine
end program
