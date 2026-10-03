! Companion to implicit_interface_77.
subroutine apply(f, y)
external f
real(8) :: y
call f(y)
end subroutine
