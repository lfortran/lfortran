! Instances of a recursive host run concurrently on OpenMP threads, and each
! instance passes the procedure value of its internal procedure `get` to
! the next one: the threads take and give back frame slots of the same
! thunk pool at the same time, and every value must reach its own instance.
! Up to 4 threads times 4 instances hold a thunk at the same time, so
! LFortran compiles it with --internal-procedure-thunks=16.
module openmp_host_assoc_03_m
implicit none
abstract interface
    integer function getter()
    end function
end interface
contains
recursive integer function host(id, depth, older) result(r)
    integer, intent(in) :: id, depth
    procedure(getter) :: older
    integer :: captured
    captured = 1000*id + depth
    r = older()
    if (depth > 1) r = r + host(id, depth - 1, get)
contains
    integer function get()
        get = captured
    end function
end function

integer function zero()
    zero = 0
end function
end module

program openmp_host_assoc_03
use openmp_host_assoc_03_m
implicit none
integer :: i, r(400), expected
!$omp parallel do num_threads(4) private(i) shared(r)
do i = 1, 400
    r(i) = host(i, 4, zero)
end do
!$omp end parallel do
do i = 1, 400
    ! host(i, 4) sums the values of the instances of depth 4, 3 and 2
    expected = 3*1000*i + 4 + 3 + 2
    if (r(i) /= expected) error stop 1
end do
print *, "ok"
end program
