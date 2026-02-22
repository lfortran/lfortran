program associate_33
! Test: associate with lbound/ubound without DIM (array result)
implicit none

real(8), allocatable :: a(:,:,:,:)

allocate( a(-3:14,1,1,1) )

associate (lb => lbound(a), ub => ubound(a))
    if (lb(1) /= -3) error stop
    if (lb(2) /= 1) error stop
    if (lb(3) /= 1) error stop
    if (lb(4) /= 1) error stop
    if (ub(1) /= 14) error stop
    if (ub(2) /= 1) error stop
    if (ub(3) /= 1) error stop
    if (ub(4) /= 1) error stop
end associate

end program associate_33
