subroutine a_proc
implicit none
integer :: i
parameter(i = 1)
print *, i
end subroutine a_proc

module parameter_02_a
implicit none
integer :: i
parameter(i = 2)
end module

program parameter_02
use parameter_02_a, only: i
implicit none
call a_proc()
print *, i
end program
