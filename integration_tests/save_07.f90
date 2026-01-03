subroutine sub(i)
real :: x(5)
integer :: i, itr
save
if (i == 1) then
x = 3.49213401
x(4) = -2134.1
else if (i == 2) then
print *, x
do itr = 1, 5
if (itr == 4) then
    if (abs(x(itr) + 2134.1) > 1e-6) error stop
else
if (abs(x(itr) - 3.49213401) > 1e-6) error stop
end if
end do
x(3) = -991.324
else
do itr = 1, 5
if (itr == 3) then
    if (abs(x(itr) + 991.324) > 1e-6) error stop
else if (itr == 4) then
    if (abs(x(itr) + 2134.1) > 1e-6) error stop
else
    if (abs(x(itr) - 3.49213401) > 1e-6) error stop
end if
end do
print *, x
end if
end subroutine

program save_07
call sub(1)
call sub(2)
call sub(3)
end program
