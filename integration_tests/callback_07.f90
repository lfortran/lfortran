program callback_07
implicit none
integer :: answer
call invoke(double_value, answer)
if (answer /= 14) error stop 1
contains
subroutine invoke(f, answer)
interface
    integer function f(n)
        integer, intent(in) :: n
    end function
end interface
integer, intent(out) :: answer
answer = f(7)
end subroutine
integer function double_value(n)
integer, intent(in) :: n
double_value = 2*n
end function
end program
