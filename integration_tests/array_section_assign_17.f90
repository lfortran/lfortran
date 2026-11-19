program array_section_assign_17
    ! The value of `m = m(2:1:-1, :)` is a section of the target, which is
    ! copied into a temporary of the shape of the section first
    implicit none
    real, allocatable :: m(:,:), m3(:,:,:)
    real :: f(2, 3)
    integer :: i, j, k

    allocate(m(0:1, 3), m3(2, 2, 2))
    do j = 1, 3
        do i = 0, 1
            m(i, j) = 10*i + j
            f(i + 1, j) = 10*i + j
        end do
    end do
    m = m(1:0:-1, :)
    do j = 1, 3
        if (m(0, j) /= 10 + j .or. m(1, j) /= j) error stop 1
    end do
    f = f(:, 3:1:-1)
    do j = 1, 3
        if (f(1, j) /= 4 - j .or. f(2, j) /= 14 - j) error stop 2
    end do
    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                m3(i, j, k) = 100*i + 10*j + k
            end do
        end do
    end do
    m3 = m3(:, 2:1:-1, :)
    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                if (m3(i, j, k) /= 100*i + 10*(3 - j) + k) error stop 3
            end do
        end do
    end do
    print *, m, f
end program
