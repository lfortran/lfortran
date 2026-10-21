module gpu_discard_guard_extremes_m
    implicit none
contains
    subroutine compare_i8(a, b, c, d, edge)
        real, intent(inout) :: a(:), b(:), c(:), d(:)
        integer(8), intent(in) :: edge
        integer :: i
        do concurrent (i = 1:size(a))
            if (i < edge) a(i) = 1
            if (edge < i) b(i) = 2
            if (edge >= i) c(i) = 3
            if (i >= edge) d(i) = 4
        end do
    end subroutine

    subroutine compare_i4(a, b, edge)
        real, intent(inout) :: a(:), b(:)
        integer(4), intent(in) :: edge
        integer :: i
        do concurrent (i = 1:size(a))
            if (i < edge) a(i) = 1
            if (i > edge) b(i) = 2
        end do
    end subroutine

    subroutine interval_i8(a, lo, hi)
        real, intent(inout) :: a(:)
        integer(8), intent(in) :: lo, hi
        integer :: i
        do concurrent (i = 0:size(a) + 1)
            if (lo < i .and. i < hi) a(i) = 5
        end do
    end subroutine

    subroutine constant_empty(a)
        real, intent(inout) :: a(:)
        integer(8), parameter :: high = huge(0_8)
        integer :: i
        do concurrent (i = 1:size(a))
            if (i > high) a(i) = 1
        end do
    end subroutine

    subroutine check(actual, expected, ok)
        real, intent(in) :: actual(:,:), expected(:,:)
        logical, intent(inout) :: ok
        ! Equality must fail for poisoned NaNs as well as finite wrong values.
        if (.not. all(actual == expected)) then
            print *, "expected:", expected
            print *, "actual:", actual
            ok = .false.
        end if
    end subroutine
end module

program gpu_discard_guard_extremes
    use gpu_discard_guard_extremes_m
    implicit none
    integer(8), parameter :: low = -huge(0_8) - 1_8, high = huge(0_8)
    integer(8), parameter :: edges8(9) = &
        [low, low + 1, 0_8, 1_8, 3_8, 4_8, 5_8, high - 1, high]
    integer(4), parameter :: edges4(5) = [-huge(0_4) - 1_4, 0_4, 3_4, 5_4, huge(0_4)]
    integer(8), parameter :: lower(5) = [0_8, 0_8, 1_8, low, high]
    integer(8), parameter :: upper(5) = [5_8, 4_8, 5_8, low, high]
    real :: actual(4,4), expected(4,4)
    integer :: i, k
    logical :: ok

    ok = .true.
    do k = 1, size(edges8)
        actual = 7
        expected = 7
        call compare_i8(actual(:,1), actual(:,2), actual(:,3), actual(:,4), edges8(k))
        do i = 1, 4
            expected(i,1) = merge(1., 7., i < edges8(k))
            expected(i,2) = merge(2., 7., i > edges8(k))
            expected(i,3) = merge(3., 7., i <= edges8(k))
            expected(i,4) = merge(4., 7., i >= edges8(k))
        end do
        call check(actual, expected, ok)
    end do

    do k = 1, size(edges4)
        actual = 7
        expected = 7
        call compare_i4(actual(:,1), actual(:,2), edges4(k))
        do i = 1, 4
            expected(i,1) = merge(1., 7., i < edges4(k))
            expected(i,2) = merge(2., 7., i > edges4(k))
        end do
        call check(actual, expected, ok)
    end do

    do k = 1, size(lower)
        actual = 7
        expected = 7
        call interval_i8(actual(:,1), lower(k), upper(k))
        do i = 1, 4
            expected(i,1) = merge(5., 7., lower(k) < i .and. i < upper(k))
        end do
        call check(actual, expected, ok)
    end do

    actual = 7
    expected = 7
    call constant_empty(actual(:,1))
    call check(actual, expected, ok)
    if (.not. ok) error stop 1
    print *, "ok"
end program
