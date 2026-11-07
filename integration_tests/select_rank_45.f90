program select_rank_45
! Pass a select rank associate name of an assumed-rank dummy, whose actual
! argument is a non-contiguous section, to explicit-shape and assumed-size
! dummies: the elements are copied in and, for intent(inout), copied out.
implicit none
integer :: v(6), m(4, 4), i
character(len=3) :: c(6)
v = [(i, i = 1, 6)]
m = reshape([(i, i = 1, 16)], [4, 4])

call check(v(1:6:2), [1, 3, 5], .false.)
call check(v(3:5), [3, 4, 5], .true.)
call check(m(1:4:2, 2:4:2), [5, 7, 13, 15], .false.)
call check(m(:, 2:3), [(i, i = 5, 12)], .true.)

call update(v(1:6:2))
if (any(v /= [11, 2, 13, 4, 15, 6])) error stop 1
call update(v(2:3))
if (any(v /= [11, 12, 23, 4, 15, 6])) error stop 2
call update(m(1:4:2, 2:4:2))
if (any(m(:, 2) /= [15, 6, 17, 8])) error stop 3
if (any(m(:, 4) /= [23, 14, 25, 16])) error stop 4
if (any(m(:, 1) /= [1, 2, 3, 4])) error stop 5
if (any(m(:, 3) /= [9, 10, 11, 12])) error stop 6

c = ['ab1', 'ab2', 'ab3', 'ab4', 'ab5', 'ab6']
call update_char(c(1:6:2))
if (any(c /= ['zz1', 'ab2', 'zz3', 'ab4', 'zz5', 'ab6'])) error stop 7
contains

subroutine check(x, expected, contiguous)
integer, intent(in) :: x(..), expected(:)
logical, intent(in) :: contiguous
integer :: n1, n2
if (is_contiguous(x) .neqv. contiguous) error stop 10
select rank (x)
rank (1)
    call explicit_shape(x, size(x), expected)
    call assumed_size(x, size(x), expected)
rank (2)
    call explicit_shape(x, size(x), expected)
    call assumed_size(x, size(x), expected)
    n1 = size(x, 1)
    n2 = size(x, 2)
    call explicit_shape_2(x, n1, n2, expected)
rank default
    error stop 11
end select
end subroutine

subroutine update(x)
integer, intent(inout) :: x(..)
select rank (x)
rank (1)
    call add_explicit_shape(x, size(x))
    call add_assumed_size(x, size(x))
rank (2)
    call add_explicit_shape(x, size(x))
    call add_assumed_size(x, size(x))
rank default
    error stop 12
end select
end subroutine

subroutine update_char(x)
character(len=*), intent(inout) :: x(..)
select rank (x)
rank (1)
    call char_assumed_size(x, size(x))
rank default
    error stop 13
end select
end subroutine

subroutine char_assumed_size(a, n)
character(len=*), intent(inout) :: a(*)
integer, intent(in) :: n
integer :: i
if (a(2) /= 'ab3') error stop 23
do i = 1, n
    a(i)(1:2) = 'zz'
end do
end subroutine

subroutine explicit_shape(a, n, expected)
integer, intent(in) :: n, a(n), expected(n)
if (any(a /= expected)) error stop 20
end subroutine

subroutine explicit_shape_2(a, n1, n2, expected)
integer, intent(in) :: n1, n2, a(n1, n2), expected(n1 * n2)
if (any(reshape(a, [n1 * n2]) /= expected)) error stop 21
end subroutine

subroutine assumed_size(a, n, expected)
integer, intent(in) :: a(*), n, expected(n)
if (any(a(1:n) /= expected)) error stop 22
end subroutine

subroutine add_explicit_shape(a, n)
integer, intent(in) :: n
integer, intent(inout) :: a(n)
a = a + 4
end subroutine

subroutine add_assumed_size(a, n)
integer, intent(inout) :: a(*)
integer, intent(in) :: n
a(1:n) = a(1:n) + 6
end subroutine

end program
