program arrays_op_33
implicit none
real :: x(4) = [1.0, 5.0, 20.0, 3.0]
logical :: l(4) = [.true., .true., .false., .false.]
logical :: m(4)
call s(4, x, l, 2.0, m)
print *, m
if (m(1) .or. .not. m(2) .or. m(3) .or. m(4)) error stop
contains
subroutine s(n, x, l, t, m)
integer, intent(in) :: n
real, intent(in) :: x(:), t
logical, intent(in) :: l(n)
logical, intent(out) :: m(n)
m = (x > t) .and. l
end subroutine
end program
