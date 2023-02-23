program main
    call vecfcn(10)

contains

subroutine vecfcn(n)
    integer, intent(in) :: n
    real :: fvec(n)

    fvec(:n) = 0.0
    if(fvec(1) /= 0.0) error stop
    if(fvec(2) /= 0.0) error stop
    if(fvec(10) /= 0.0) error stop
    fvec(2:) = 1.0
    if (fvec(1) /= 0.0) error stop
    if (fvec(2) /= 1.0) error stop
    if (fvec(10) /= 1.0) error stop
    fvec(:) = 2.0
    print *, fvec
    if (fvec(1) /= 2.0) error stop
    if (fvec(2) /= 2.0) error stop
    if (fvec(10) /= 2.0) error stop

end subroutine vecfcn

end program main
