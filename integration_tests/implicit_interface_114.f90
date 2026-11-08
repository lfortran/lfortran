program implicit_interface_114
integer :: i
i = 1
call apply(incr, i)
if (i /= 2) error stop
contains
subroutine incr(x)
    integer, intent(inout) :: x
    x = x + 1
end subroutine
subroutine apply(f, x)
    external f
    integer, intent(inout) :: x
    call f(x)
end subroutine
end program
