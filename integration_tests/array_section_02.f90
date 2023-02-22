program main
    call vecfcn(10)

contains

subroutine vecfcn(n)
    integer, intent(in) :: n
    real :: fvec(n)

    fvec(:n) = 0.0
    fvec(2:) = 1.0
    fvec(:) = 2.0
    print *, fvec

end subroutine vecfcn

end program main
