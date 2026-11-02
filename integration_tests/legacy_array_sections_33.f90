! With --legacy-array-sections, an array element passed to an array dummy
! is sequence associated with the elements that follow it in memory, while
! a non-contiguous section such as a(1, :) is still copied in and back out
program legacy_array_sections_33
    implicit none
    integer :: a(2, 3), b(3, 3), i

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call bump(a(1, :), 3)
    if (any(a /= reshape([11, 2, 13, 4, 15, 6], [2, 3]))) error stop 1

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call bump_assumed_size(a(2, :), 3)
    if (any(a /= reshape([1, 12, 3, 14, 5, 16], [2, 3]))) error stop 2

    b = 0
    call bump(b(2, 2), 3)
    if (any(b /= reshape([0, 0, 0, 0, 10, 10, 10, 0, 0], [3, 3]))) error stop 3

    b = 0
    call set_val(b(2, 2), 3, 7)
    if (b(2, 3) /= 7) error stop 4
    if (count(b /= 0) /= 1) error stop 5

    b = reshape([(i, i = 1, 9)], [3, 3])
    call scale2(b(2:3, 2:3))
    if (any(b /= reshape([1, 2, 3, 4, 10, 12, 7, 16, 18], [3, 3]))) error stop 6

    print *, "ok"

contains

    subroutine bump(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = x(i) + 10
        end do
    end subroutine

    subroutine bump_assumed_size(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(*)
        x(1:n) = x(1:n) + 10
    end subroutine

    subroutine scale2(x)
        integer, intent(inout) :: x(2, 2)
        x = 2 * x
    end subroutine

    subroutine set_val(c, ldc, val)
        integer, intent(in) :: ldc, val
        integer, intent(inout) :: c(ldc, *)
        c(1, 2) = val
    end subroutine

end program
