! Companion to implicit_interface_76.
real function lib(p, a)
external p
real :: a(10)
call p(a)
lib = a(1)
end function
