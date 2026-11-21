! An external procedure with an assumed-shape dummy and a saved local,
! whose dummy procedure takes its interface from an internal procedure,
! called directly, with a section, through a dummy procedure and through a
! procedure pointer: all calls share one saved variable.
subroutine save_36_cnt(a, f, n)
double precision, intent(in) :: a(:)
procedure(g) :: f
integer, intent(out) :: n
integer, save :: k = 0
k = k + 1
n = k + 0 * f(1)
contains
    integer function g(i)
    integer, intent(in) :: i
    g = i
    end function
end subroutine

integer function save_36_h(i)
integer, intent(in) :: i
save_36_h = i
end function

program save_37
implicit none
interface
    subroutine save_36_cnt(a, f, n)
    double precision, intent(in) :: a(:)
    interface
        integer function f(i)
        integer, intent(in) :: i
        end function
    end interface
    integer, intent(out) :: n
    end subroutine
    integer function save_36_h(i)
    integer, intent(in) :: i
    end function
end interface
procedure(save_36_cnt), pointer :: p
double precision :: a(3)
integer :: n
a = 1
call save_36_cnt(a, save_36_h, n)
if (n /= 1) error stop 1
call run(save_36_cnt, n)
if (n /= 2) error stop 2
p => save_36_cnt
call p(a, save_36_h, n)
if (n /= 3) error stop 3
call save_36_cnt(a(1:3:2), save_36_h, n)
if (n /= 4) error stop 4
call save_36_cnt(a, save_36_h, n)
if (n /= 5) error stop 5
print *, n
contains
    subroutine run(f, n)
    procedure(save_36_cnt) :: f
    integer, intent(out) :: n
    double precision :: b(2)
    b = 2
    call f(b, save_36_h, n)
    end subroutine
end program
