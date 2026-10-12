subroutine intrinsics_484_sub(r, t, rate)
implicit none
real, intent(out) :: r, t
integer, intent(out) :: rate
integer :: c
call cpu_time(t)
call random_number(r)
call system_clock(c, rate)
if (t < 0) error stop
if (c < 0) error stop
end subroutine
