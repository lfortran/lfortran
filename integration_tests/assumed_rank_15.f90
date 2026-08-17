program assumed_rank_15
! An array element (scalar) actual argument passed to an assumed-rank
! dummy must be accepted and select the rank 0 case.
implicit none
integer :: x(3) = [10, 20, 30]
integer :: s = 7

call check(x(2), 0, 20)
call check(s, 0, 7)
call check(x, 1, 10)
call check(sum(x), 0, 60)
print *, "ok"

contains

subroutine check(a, expected_rank, expected_value)
integer, intent(in) :: a(..)
integer, intent(in) :: expected_rank, expected_value
if (rank(a) /= expected_rank) error stop 1
select rank(a)
rank (0)
    if (expected_rank /= 0) error stop 2
    if (a /= expected_value) error stop 3
rank (1)
    if (expected_rank /= 1) error stop 4
    if (a(1) /= expected_value) error stop 5
rank default
    error stop 6
end select
end subroutine check

end program assumed_rank_15
