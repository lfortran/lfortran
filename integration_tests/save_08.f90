program save_08

call sub(1)
call sub(2)
call sub(3)

contains

subroutine sub(i)
integer :: x(5)
integer, intent(in) :: i
save
if (i == 1) then
    x = 1
else if ( i == 2) then
    print *, x
    if (any(x /= 1)) error stop
    x = x + 1
else
    print *, x
    if (any(x /= 2)) error stop
end if

end subroutine

end program
