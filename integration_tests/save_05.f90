subroutine f(i)
implicit none
integer, save :: small
integer, intent(in) :: i
if (i == 1) then
    small = 12
else if (i == 2) then
    if (small /= 12) error stop
end if
print *, small
end subroutine

subroutine g(i)
implicit none
integer :: big
integer, intent(in) :: i
if (i == 1) then
    big = 6
end if
print *, big
end subroutine

program save_05
implicit none
call f(1)
call g(1)
call f(2)
end program
