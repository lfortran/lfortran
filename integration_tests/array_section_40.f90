! Non-contiguous sections of fixed-size arrays passed to explicit-shape
! and assumed-size dummies are copied in and copied back out
module array_section_40_mod
    implicit none
contains
    subroutine add_ten(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = x(i) + 10
        end do
    end subroutine

    subroutine add_ten_assumed_size(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(*)
        x(1:n) = x(1:n) + 10
    end subroutine
end module

program array_section_40
    use array_section_40_mod
    implicit none
    integer :: a(2, 3), b(6), s
    real :: c(3, 3)
    integer :: i

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call bump(a(1, :), 3)
    if (any(a /= reshape([11, 2, 13, 4, 15, 6], [2, 3]))) error stop 1

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call add_ten(a(2, :), 3)
    if (any(a /= reshape([1, 12, 3, 14, 5, 16], [2, 3]))) error stop 2

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call add_ten_assumed_size(a(1, :), 3)
    if (any(a /= reshape([11, 2, 13, 4, 15, 6], [2, 3]))) error stop 3

    b = [1, 2, 3, 4, 5, 6]
    call bump3(b(1:5:2))
    if (any(b /= [11, 2, 13, 4, 15, 6])) error stop 4

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    s = total(a(1, :))
    if (s /= 9) error stop 5

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call fill(a(2, :))
    if (any(a /= reshape([1, 7, 3, 8, 5, 9], [2, 3]))) error stop 6

    c = reshape([(real(i), i = 1, 9)], [3, 3])
    call scale2(c(2:3, 2:3))
    if (any(c /= reshape([1., 2., 3., 4., 10., 12., 7., 16., 18.], [3, 3]))) error stop 7

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

    subroutine bump3(x)
        integer, intent(inout) :: x(3)
        x = x + 10
    end subroutine

    integer function total(x)
        integer, intent(in) :: x(3)
        total = x(1) + x(2) + x(3)
    end function

    subroutine fill(x)
        integer, intent(out) :: x(3)
        x = [7, 8, 9]
    end subroutine

    subroutine scale2(x)
        real, intent(inout) :: x(2, 2)
        x = 2 * x
    end subroutine

end program
