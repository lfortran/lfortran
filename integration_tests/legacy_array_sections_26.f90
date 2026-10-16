! An array element passed to an explicit-shape or assumed-size dummy gives it
! the element and the elements that follow it in the actual's storage, which
! the procedure indexes with its own shape, also when the actual has more
! dimensions than the dummy or the dummy is shorter, and the actual is a
! fixed-size or an explicit-shape array. A section written in the source,
! such as `m(2:, 2:)`, gives the dummy only its own elements instead.
module legacy_array_sections_26_m
    implicit none
contains
    subroutine mod_set(c, ldc, val)
        integer, intent(in) :: ldc
        real, intent(inout) :: c(ldc, *)
        real, intent(in) :: val
        c(1, 2) = val
    end subroutine mod_set

    subroutine mod_fill(n, y)
        integer, intent(in) :: n
        real, intent(out) :: y(n)
        integer :: i
        do i = 1, n
            y(i) = 100 + i
        end do
    end subroutine mod_fill
end module legacy_array_sections_26_m

program legacy_array_sections_26
    use legacy_array_sections_26_m
    implicit none
    real :: a(3, 3), m(3, 3), ys(6)
    integer :: i

    a = 0
    call set_assumed(a(2, 2), 3, 7.0)
    call check(a, 8, 7.0, 1)
    a = 0
    call set_explicit(a(2, 1), 3, 8.0)
    call check(a, 7, 8.0, 2)
    a = 0
    call fill_assumed(4, a(2, 2))
    if (any(a(2:3, 2) /= [11, 12]) .or. any(a(1:2, 3) /= [13, 14])) error stop 3
    if (any(a(:, 1) /= 0) .or. a(1, 2) /= 0 .or. a(3, 3) /= 0) error stop 4
    a = 0
    call fill_explicit(4, a(2, 2))
    if (any(a(2:3, 2) /= [21, 22]) .or. any(a(1:2, 3) /= [23, 24])) error stop 5
    if (any(a(:, 1) /= 0) .or. a(1, 2) /= 0 .or. a(3, 3) /= 0) error stop 6
    a = 0
    call mod_set(a(2, 2), 3, 9.0)
    call check(a, 8, 9.0, 7)
    a = 0
    call mod_fill(2, a(3, 1))
    if (a(3, 1) /= 101 .or. a(1, 2) /= 102 .or. sum(a) /= 203) error stop 8
    a = reshape([(real(i), i = 1, 9)], [3, 3])
    if (total(4, a(2, 2)) /= 26) error stop 9

    a = 0
    call from_explicit_shape(a, 3)
    if (a(2, 3) /= 6 .or. a(3, 1) /= 11 .or. a(1, 2) /= 12 .or. sum(a) /= 29) error stop 10

    m = 0
    call set_block(2, 2, m(2:, 2:))
    if (any(m(2:3, 2:3) /= reshape([11, 21, 12, 22], [2, 2]))) error stop 11
    if (any(m(1, :) /= 0) .or. any(m(:, 1) /= 0)) error stop 12
    ys = [1, 10, 2, 20, 3, 30]
    call fill_explicit(3, ys(1::2))
    if (any(ys /= [21, 10, 22, 20, 23, 30])) error stop 13
    print '(a)', 'legacy_array_sections_26 ok'
contains
    subroutine check(x, k, val, code)
        real, intent(in) :: x(3, 3)
        integer, intent(in) :: k, code
        real, intent(in) :: val
        real :: flat(9)
        flat = reshape(x, [9])
        if (flat(k) /= val .or. count(flat /= 0) /= 1) error stop code
    end subroutine check

    subroutine set_assumed(c, ldc, val)
        integer, intent(in) :: ldc
        real, intent(inout) :: c(ldc, *)
        real, intent(in) :: val
        c(1, 2) = val
    end subroutine set_assumed

    subroutine set_explicit(c, ldc, val)
        integer, intent(in) :: ldc
        real, intent(inout) :: c(ldc, 2)
        real, intent(in) :: val
        c(3, 2) = val
    end subroutine set_explicit

    subroutine fill_assumed(n, y)
        integer, intent(in) :: n
        real, intent(out) :: y(*)
        integer :: i
        do i = 1, n
            y(i) = 10 + i
        end do
    end subroutine fill_assumed

    subroutine fill_explicit(n, y)
        integer, intent(in) :: n
        real, intent(out) :: y(n)
        integer :: i
        do i = 1, n
            y(i) = 20 + i
        end do
    end subroutine fill_explicit

    real function total(n, y)
        integer, intent(in) :: n
        real, intent(in) :: y(n)
        total = sum(y)
    end function total

    subroutine from_explicit_shape(d, ld)
        integer, intent(in) :: ld
        real, intent(inout) :: d(ld, 3)
        call set_assumed(d(2, 2), ld, 6.0)
        call fill_assumed(2, d(3, 1))
    end subroutine from_explicit_shape

    subroutine set_block(m, n, x)
        integer, intent(in) :: m, n
        real, intent(out) :: x(m, n)
        integer :: i, j
        do j = 1, n
            do i = 1, m
                x(i, j) = 10 * i + j
            end do
        end do
    end subroutine set_block
end program legacy_array_sections_26
