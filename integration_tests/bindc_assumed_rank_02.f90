! SIZE and SHAPE of an assumed-rank dummy of a bind(c) procedure (#16221)
module bindc_assumed_rank_02_m
use iso_c_binding, only: c_char, c_float, c_int
implicit none
contains

subroutine check(c, n1, n, last) bind(c)
    real(c_float), intent(in) :: c(..)
    integer(c_int), value :: n1, n
    real(c_float), value :: last
    if (size(c, 1) /= n1) error stop 1
    if (size(c) /= n) error stop 2
    if (product(shape(c)) /= n) error stop 3
    select rank (c)
    rank (2)
        if (c(n1, size(c, 2)) /= last) error stop 4
    rank default
        error stop 5
    end select
end subroutine

subroutine check_char(t, n) bind(c)
    character(kind=c_char, len=*), intent(in) :: t(..)
    integer(c_int), value :: n
    if (size(t) /= n) error stop 6
end subroutine

subroutine check_alloc(c, n) bind(c)
    real(c_float), allocatable, intent(inout) :: c(..)
    integer(c_int), value :: n
    if (size(c) /= n) error stop 7
end subroutine

subroutine grow(c) bind(c)
    real(c_float), allocatable, intent(inout) :: c(..)
    select rank (c)
    rank (1)
        deallocate(c)
        allocate(c(7))
        c = 1.0
    end select
end subroutine

subroutine check_opt(o, n) bind(c)
    real(c_float), optional, intent(in) :: o(..)
    integer(c_int), value :: n
    if (.not. present(o)) error stop 12
    if (size(o) /= n) error stop 13
end subroutine

subroutine fwd(a, n1, n, last)
    real(c_float), intent(in) :: a(..)
    integer(c_int), value :: n1, n
    real(c_float), value :: last
    call check(a, n1, n, last)
end subroutine

subroutine fwd_alloc(a, n)
    real(c_float), allocatable, intent(inout) :: a(..)
    integer(c_int), value :: n
    call check_alloc(a, n)
    if (size(a) /= n) error stop 8
end subroutine

end module

program bindc_assumed_rank_02
use bindc_assumed_rank_02_m
implicit none
real(c_float) :: x(3, 4)
real(c_float), allocatable :: z(:, :), g(:)
character(len=2) :: w(3)
integer :: i
x = reshape([(real(i), i = 1, 12)], [3, 4])
w = ["ab", "cd", "ef"]
call check(x, 3, 12, 12.0)
! An assumed-rank actual passed on to a bind(c) procedure
call fwd(x, 3, 12, 12.0)
call check_char(w, 3)
call check_opt(x, 12)
allocate(z(2, 3))
call fwd_alloc(z, 6)
if (any(shape(z) /= [2, 3])) error stop 9
! The bind(c) procedure gives back an array of a new size
allocate(g(2))
call grow(g)
if (size(g) /= 7) error stop 10
if (sum(g) /= 7.0) error stop 11
print *, "ok"
end program
