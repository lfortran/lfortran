program do_loop_10
! Test that DO-loop variable retains the correct final value after loop
! completion (one step beyond the last iteration value per Fortran standard).
implicit none
integer :: i, j, k

! Positive step: do i = 100, 105, 2
! Iterations: i=100, 102, 104. Then i incremented to 106 > 105, exit.
do i = 100, 105, 2
end do
if (i /= 106) error stop

! Step of 1 (default): do j = 1, 5
! Iterations: j=1,2,3,4,5. Then j incremented to 6 > 5, exit.
do j = 1, 5
end do
if (j /= 6) error stop

! Negative step: do k = 10, 1, -3
! Iterations: k=10, 7, 4, 1. Then k decremented to -2 < 1, exit.
do k = 10, 1, -3
end do
if (k /= -2) error stop

print *, "ok"
end program
