module value_extent_m
    implicit none
contains
    subroutine total(x, y)
        real, intent(in) :: x(:,:)
        real, intent(out) :: y
        integer :: i, j
        y = 0
        do j = 1, size(x, 2)
            do i = 1, size(x, 1)
                y = y + x(i,j)
            end do
        end do
    end subroutine

    subroutine optional_total(y, x)
        real, intent(out) :: y
        real, optional, intent(in) :: x(:,:)
        y = -1
        if (present(x)) call total(x, y)
    end subroutine
end module

program check_value_extents
    use value_extent_m
    implicit none
    real, allocatable :: x(:,:)
    real :: y
    integer :: i, j
    allocate(x(-1:2,3:5))
    do j = 3, 5
        do i = -1, 2
            x(i,j) = i + 2*j
        end do
    end do
    call total(x, y)
    if (y /= 102) error stop 1
    call total(x(-1:2:2,3:5:2), y)
    if (y /= 32) error stop 2
    call total(x(1:0,:), y)
    if (y /= 0) error stop 3
    call optional_total(y)
    if (y /= -1) error stop 4
    call optional_total(y, x(:,3:5:2))
    if (y /= 68) error stop 5
end program
