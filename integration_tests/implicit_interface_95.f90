! An OpenMP task with shared(t) calls an external subroutine with an implicit
! interface that sets t (#12873).
program implicit_interface_95
implicit none
external :: osub
real :: t
t = 0
!$omp parallel
!$omp single
!$omp task shared(t)
call osub(t)
!$omp end task
!$omp end single
!$omp end parallel
print *, t
if (t /= 42.0) error stop 1
end program

subroutine osub(t)
real :: t
t = 42.0
end subroutine
