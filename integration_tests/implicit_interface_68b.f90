! Companion to implicit_interface_68.
subroutine apply(f, y)
external f
real(8) :: y
call f(y)
end subroutine

subroutine ext_other(x)
real(8) :: x
x = 5
end subroutine
