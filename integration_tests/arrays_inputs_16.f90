! An allocatable actual argument is always contiguous, so it is passed to an
! assumed-shape or explicit-shape dummy argument as it is.
module arrays_inputs_16_mod
implicit none
contains

    subroutine check_values(r, expected)
        integer, intent(in) :: r(:), expected(:)
        if (size(r) /= size(expected)) error stop "size"
        if (any(r /= expected)) error stop "values"
    end subroutine check_values

    logical function has_true(x)
        logical, intent(in) :: x(:)
        integer :: i
        has_true = .false.
        do i = 1, size(x)
            if (x(i)) has_true = .true.
        end do
    end function has_true

    integer function total(x, n)
        integer, intent(in) :: n
        integer, intent(in) :: x(n)
        integer :: i
        total = 0
        do i = 1, n
            total = total + x(i)
        end do
    end function total

    subroutine twice(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = 2 * x(i)
        end do
    end subroutine twice

end module arrays_inputs_16_mod

program arrays_inputs_16
use arrays_inputs_16_mod, only: check_values, has_true, total, twice
implicit none
integer, allocatable :: a(:)
logical, allocatable :: l(:)
allocate(a(4))
a = [1, 2, 3, 4]
call check_values(a, [1, 2, 3, 4])
call twice(a, 4)
call check_values(a, [2, 4, 6, 8])
if (total(a, 4) /= 20) error stop "total"
allocate(l(3))
l = [.false., .true., .false.]
if (.not. has_true(l)) error stop "has_true"
if (.not. any(l)) error stop "any"
l(2) = .false.
if (has_true(l)) error stop "has_true false"
if (any(l)) error stop "any false"
print *, a
end program arrays_inputs_16
