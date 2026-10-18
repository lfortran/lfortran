program pass_array_by_data_15
implicit none
call work([1.0, -1.0, 2.0])
print *, "ok"
contains
subroutine show(c)
real, intent(in) :: c(:)
if (size(c) /= 2) error stop
if (abs(sum(c) - 3.0) > 1e-6) error stop
end subroutine
subroutine work(xl)
real, intent(in) :: xl(:)
real :: c(count(xl > 0.0))
c = [1.0, 2.0]
call show(c)
end subroutine
end program
