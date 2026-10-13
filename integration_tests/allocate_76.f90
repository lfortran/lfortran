module allocate_76_util
    implicit none
contains
    subroutine set_alloc(xx, yy)
        real, intent(in) :: xx(:)
        real, allocatable, intent(out) :: yy(:)

        if (allocated(yy)) error stop
        allocate(yy(size(xx)))
        yy = xx
    end subroutine set_alloc

    subroutine select_vec(iuse, xx)
        integer, intent(in) :: iuse(:)
        real, allocatable, intent(inout) :: xx(:)

        ! The input temporary must survive deallocation on callee entry.
        call set_alloc(xx(iuse), xx)
    end subroutine select_vec
end module allocate_76_util

program allocate_76
    use allocate_76_util
    implicit none
    real, allocatable :: x(:)
    integer :: i

    allocate(x(3))
    do i = 1, 3
        x(i) = 10 * real(i)
    end do
    call select_vec([1, 3], x)

    if (.not. allocated(x)) error stop
    if (size(x) /= 2) error stop
    if (any(x /= [10.0, 30.0])) error stop
end program allocate_76
