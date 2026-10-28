! PRESENT() with present and absent optional arguments.
program optional_19
implicit none
print *, square()
if (square() /= 1) error stop 1
print *, square(4)
if (square(4) /= 16) error stop 2
contains
integer function square(x)
integer, intent(in), optional :: x
if (present(x)) then
square = x*x
else
square = 1
end if
end function square
end program optional_19
