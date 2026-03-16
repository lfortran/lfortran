program allocate_41
    ! Test realloc-lhs preserves lower bounds from RHS
    implicit none
    integer :: i, ng, nz, ni, nf
    real(8), allocatable :: x(:,:), y(:,:)

    ng = 4
    nz = 12

    ni = 1 - ng
    nf = nz + ng

    allocate( x(ni:nf, 3) )
    x = 1.d0
    y = x

    if (lbound(y, 1) /= ni) error stop
    if (ubound(y, 1) /= nf) error stop
    if (lbound(y, 2) /= 1) error stop
    if (ubound(y, 2) /= 3) error stop

    do i = ni, nf
        if (abs(y(i, 1) - 1.d0) > 1.d-15) error stop
    end do

    print *, "PASS"
end program allocate_41
