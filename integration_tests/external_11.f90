subroutine exabs()
external zabs
complex zresult

zresult = zabs(1.0, -1.0)
print *, abs(zresult)
if ( abs(abs(zresult) - 1.41421354) > 1e-8 ) error stop
end subroutine

real function zabs (x, y)
real x, y
zabs = (x**2 + y**2)**0.5
end function

program external_11
call exabs()
end program
