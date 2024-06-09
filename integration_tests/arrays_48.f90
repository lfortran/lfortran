program arrays_48
implicit none
integer :: cs(6)
integer :: nx = 4
cs = [1, 2, 3, 4, 5, 6]
call f(cs)

contains

subroutine f(x)
integer, intent(in) :: x(nx)
print *, x
if(sum(x) /= 10) error stop
end subroutine

end program
