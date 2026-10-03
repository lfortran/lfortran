program cpp_pre_13
implicit none
! Test null preprocessing directive (C99 6.10.7 / C23 6.10.9)
! A line with just "#" or "# /* comment */" should be silently ignored.
#
# /* this is a CPP comment. It is erased regardless of the base language */
integer :: x
x = 42
if (x /= 42) error stop
print *, "PASSED"
end program
