module realloc_lhs_29_util
    implicit none
contains
    subroutine set_alloc(xx, yy)
        real, intent(in) :: xx(:)
        real, allocatable, intent(out) :: yy(:)

        yy = xx
    end subroutine set_alloc

    subroutine select_vec(iuse, xx)
        integer, intent(in) :: iuse(:)
        real, allocatable, intent(inout) :: xx(:)

        ! Evaluate the vector input before deallocating the output actual.
        call set_alloc(xx(iuse), xx)
    end subroutine select_vec
end module realloc_lhs_29_util

program realloc_lhs_29
    use realloc_lhs_29_util
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
end program realloc_lhs_29
