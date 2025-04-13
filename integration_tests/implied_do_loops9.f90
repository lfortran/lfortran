program implied_do_loops9 
    integer, parameter :: n = 3
    complex :: b(n,n)
    complex, parameter :: i_ = cmplx(0,1)
    integer :: i,j
    b = reshape([((merge(i + 1*i_,0*i_,i==j), i=1,n), j=1,n)], [n,n])
    print * , b
    if (b(1,1) /= cmplx(1,1)) error stop
    if (b(1,2) /= cmplx(0,0)) error stop
    if (b(1,3) /= cmplx(0,0)) error stop
    if (b(2,1) /= cmplx(0,0)) error stop
    if (b(2,2) /= cmplx(2,1)) error stop
    if (b(2,3) /= cmplx(0,0)) error stop
    if (b(3,1) /= cmplx(0,0)) error stop
    if (b(3,2) /= cmplx(0,0)) error stop
    if (b(3,3) /= cmplx(3,1)) error stop
end program