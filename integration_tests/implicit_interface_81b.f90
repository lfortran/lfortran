! Companion to implicit_interface_81.
subroutine lib(f, y)
external f
real(8) :: y
call f(y)
end subroutine
