program select_rank_44
! Pass a select rank associate name of an assumed-rank dummy to an
! assumed-size dummy (sequence association).
implicit none
real :: r(3), r2(3, 2)
call si([8, 9])
call si(reshape([1, 2, 3, 4], [2, 2]))
r = [1.5, 2.5, 3.0]
call sr(r)
r2 = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2])
call sr(r2)
if (abs(r(3) - 6.0) > 1e-6) error stop
if (abs(r2(2, 1) - 4.0) > 1e-6) error stop
contains

subroutine si(x)
integer, intent(in) :: x(..)
select rank (x)
rank (1)
    call gi(x, size(x), 17)
rank (2)
    call gi(x, size(x), 10)
rank default
    error stop
end select
end subroutine

subroutine gi(a, n, expected)
integer, intent(in) :: a(*), n, expected
if (sum(a(1:n)) /= expected) error stop
if (a(1) /= 8 .and. a(1) /= 1) error stop
end subroutine

subroutine sr(x)
real, intent(inout) :: x(..)
select rank (x)
rank (1)
    call gr(x, size(x), 7.0)
    if (abs(x(1) - 3.0) > 1e-6) error stop
rank (2)
    call gr(x, size(x), 21.0)
    if (abs(x(1, 1) - 2.0) > 1e-6) error stop
    if (abs(x(3, 2) - 12.0) > 1e-6) error stop
rank default
    error stop
end select
end subroutine

subroutine gr(a, n, expected)
real, intent(inout) :: a(*)
integer, intent(in) :: n
real, intent(in) :: expected
if (abs(sum(a(1:n)) - expected) > 1e-5) error stop
a(1:n) = 2.0 * a(1:n)
end subroutine

end program
