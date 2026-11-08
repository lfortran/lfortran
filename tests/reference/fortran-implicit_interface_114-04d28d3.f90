program implicit_interface_114
implicit none
integer(4) :: i
i = 1
call apply(incr, i)
if (i /= 2) then
    error stop
end if

contains

subroutine apply(f, x)
    integer(4), intent(inout) :: x
    external :: f
    call f(x)
end subroutine apply

subroutine incr(x)
    integer(4), intent(inout) :: x
    x = x + 1
end subroutine incr

end program implicit_interface_114
