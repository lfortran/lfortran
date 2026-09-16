! Companion to implicit_interface_74.
subroutine lib(p)
external p
character(len=5) :: c
call p(c, 5)
if (c /= "hello") error stop
end subroutine
