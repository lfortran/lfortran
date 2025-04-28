subroutine xyzzy (i)
integer :: i
i = max (i, ifunc (42))
print *, ifunc (42)
end subroutine

subroutine xyyzzy (i)
integer :: i
i = dim (i, ifunc (42))
print *, ifunc (42)
end subroutine

program implicit_interface_19
integer :: i
i = 1245
call xyzzy (i)
print *, i
if ( i /= 38304 ) error stop
call xyyzzy (i)
print *, i
if ( i /= 0 ) error stop
end program

