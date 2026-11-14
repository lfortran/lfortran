program openmp_118
! OpenMP conditional compilation: `!$` lines are compiled with OpenMP and are
! comments without it
implicit none
logical :: on
integer :: k
character(20) :: s
on = .false.
!$ on = .true.

k = 1 + &
!$ 2 + &
3
s = 'ab&
!$&cd&
&ef'
if (on) then
    if (k /= 6) error stop
    if (s /= "abcdef") error stop
else
    if (k /= 4) error stop
    if (s /= "abef") error stop
end if
print *, on, k, s
end program
