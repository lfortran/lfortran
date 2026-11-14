! The rank of a `select rank` selector is only known inside its rank block:
! it neither applies after `end select` nor to a same-named assumed-rank
! dummy of a later procedure.
module select_rank_47_m
implicit none
contains
integer function rank_after(x) result(r)
    integer, intent(in) :: x(..)
    select rank (x)
    rank (1)
        r = -1
    end select
    r = rank(x)
end function

integer function rank_of(x) result(r)
    integer, intent(in) :: x(..)
    r = rank(x)
end function

real function total(v)
    real, intent(in) :: v(..)
    total = 0.0
    select rank (v)
    rank (1)
        total = sum(v)
    end select
end function

subroutine count_dims(n, v)
    integer, intent(out) :: n
    real, intent(in), optional :: v(..)
    n = -1
    if (present(v)) n = rank(v)
end subroutine
end module

program select_rank_47
use select_rank_47_m
implicit none
integer :: a(2,3), n
real :: b(3)
a = 1
b = 1.0
if (rank_after(a) /= 2) error stop 1
if (rank_of(a) /= 2) error stop 2
call count_dims(n)
if (n /= -1) error stop 3
call count_dims(n, b)
if (n /= 1) error stop 4
if (abs(total(b) - 3.0) > 1e-6) error stop 5
print *, "ok"
end program
