program pointer_11
    implicit none
    real, allocatable, target :: a(:)
    real, pointer :: p(:)
    integer :: i

    allocate(a(5))
    do i = 1, 5
        a(i) = real(i * 10)
    end do

    ! Pointer bounds remapping: p(1:5) => a
    p(1:5) => a
    if (size(p) /= 5) error stop
    do i = 1, 5
        if (abs(p(i) - a(i)) > 1.0e-6) error stop
    end do

    ! Pointer bounds remapping with different lower bound: p(0:4) => a
    p(0:4) => a
    if (size(p) /= 5) error stop
    if (lbound(p, 1) /= 0) error stop
    if (ubound(p, 1) /= 4) error stop
    do i = 0, 4
        if (abs(p(i) - a(i + 1)) > 1.0e-6) error stop
    end do

    ! Pointer bounds remapping to a subsection: p(1:3) => a(1:3)
    p(1:3) => a(1:3)
    if (size(p) /= 3) error stop
    do i = 1, 3
        if (abs(p(i) - a(i)) > 1.0e-6) error stop
    end do

    print *, "PASS"
end program pointer_11
