subroutine vecfcn(n)
    implicit none
    integer :: n
    real :: fvec(n)

    fvec(1) = 1
    fvec(2:n) = 0.0
    print *, fvec
    if (fvec(1) /= 1.0) error stop
    if (fvec(2) /= 0.0) error stop
    if (fvec(10) /= 0.0) error stop

end subroutine vecfcn

program main
    call vecfcn(10)
end program main
