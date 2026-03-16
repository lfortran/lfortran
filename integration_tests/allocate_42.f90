program allocate_42
    ! Test that allocate with mold= and source= preserves lower bounds
    implicit none
    integer :: i, ng, nz, ni, nf
    real(8), allocatable :: x(:,:), dx(:,:), y(:,:)

    ng = 4
    nz = 12

    ni = 1 - ng
    nf = nz + ng

    allocate( x(ni:nf, 3) )
    allocate( dx, mold = x )

    dx = 1.d0

    if (lbound(dx, 1) /= ni) error stop
    if (ubound(dx, 1) /= nf) error stop
    if (lbound(dx, 2) /= 1) error stop
    if (ubound(dx, 2) /= 3) error stop

    do i = ni, nf
        if (abs(dx(i, 1) - 1.d0) > 1.d-15) error stop
    end do

    ! Test that allocate with source= preserves lower bounds and values
    x = 1.d0
    allocate( y, source = x )

    if (lbound(y, 1) /= ni) error stop
    if (ubound(y, 1) /= nf) error stop
    if (lbound(y, 2) /= 1) error stop
    if (ubound(y, 2) /= 3) error stop

    do i = ni, nf
        if (abs(y(i, 1) - 1.d0) > 1.d-15) error stop
    end do

    print *, "PASS"
end program allocate_42
