program callback_08
implicit none
integer :: a(3)
a = [10, 20, 30]
call invoke(double_value, -1, a)
if (any(a /= [10, 14, 30])) error stop 1
a = [10, 20, 30]
call invoke(double_value, 4, a)
if (any(a /= [10, 14, 30])) error stop 2
contains
subroutine invoke(f, lo, x)
interface
    integer function f(n)
        integer, intent(in) :: n
    end function
end interface
integer, intent(in) :: lo
integer, intent(inout) :: x(lo:)
if (lbound(x, 1) /= lo .or. size(x) /= 3) error stop 3
x(lo + 1) = f(7)
end subroutine
integer function double_value(n)
integer, intent(in) :: n
double_value = 2*n
end function
end program
