subroutine vecfcn(n)
    implicit none
    integer :: n
    real :: fvec(n)

    fvec(2:n) = 0.0
    print *, fvec

end subroutine vecfcn

program main
    call vecfcn(10)
end program main
