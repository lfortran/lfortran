program intrinsics_484
! Both translation units instantiate the same `cpu_time`, `random_number`
! and `system_clock` helpers; they must link together (#14496, #11642).
implicit none
interface
    subroutine intrinsics_484_sub(r, t, rate)
        real, intent(out) :: r, t
        integer, intent(out) :: rate
    end subroutine
end interface
real :: r, t1, t2, x
integer :: c, rate, rate2
call cpu_time(t1)
call random_number(x)
call system_clock(c, rate)
call intrinsics_484_sub(r, t2, rate2)
call cpu_time(t2)
print *, x, r, rate, rate2
if (x < 0 .or. x >= 1) error stop
if (r < 0 .or. r >= 1) error stop
if (t1 < 0 .or. t2 < t1) error stop
if (rate <= 0 .or. rate2 <= 0) error stop
if (c < 0) error stop
end program
