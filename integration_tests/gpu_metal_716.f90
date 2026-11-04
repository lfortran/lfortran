program gpu_metal_716
    ! An allocatable component of an array of a derived type reaches the
    ! kernel as the components' data laid end to end, where each element's
    ! component starts in it, and how far each one runs. Components of
    ! different lengths, one of them empty and one not allocated, so every
    ! element starts somewhere else in the packed data; the kernel updates
    ! the last element of each component and the host checks that every
    ! element landed back in its own component.
    implicit none
    type :: t
        integer, allocatable :: v(:)
    end type
    type(t) :: x(4)
    integer :: i, j, n(4)
    n = [5, 0, 12, 7]
    do i = 1, 4
        if (i == 2) cycle
        allocate(x(i)%v(n(i)))
        do j = 1, n(i)
            x(i)%v(j) = 100*i + j
        end do
    end do
    allocate(x(2)%v(0))
    do concurrent (i = 1:4, n(i) > 0)
        x(i)%v(n(i)) = x(i)%v(n(i)) + 1000*i
    end do
    print *, x(1)%v(n(1)), x(3)%v(n(3)), x(4)%v(n(4))
    if (size(x(2)%v) /= 0) error stop 1
    do i = 1, 4
        do j = 1, n(i) - 1
            if (x(i)%v(j) /= 100*i + j) error stop 2
        end do
        if (n(i) > 0) then
            if (x(i)%v(n(i)) /= 1100*i + n(i)) error stop 3
        end if
    end do
end program
