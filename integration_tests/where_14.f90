program where_14
    real :: A(4)
    call temp(A)
    
    contains
    subroutine temp(A)
    real, intent(in) :: A(:)
    integer :: xbdi(size(A))
    integer :: ssq(size(A))
    xbdi = [212334,0,212121,0]
    ssq = [1,10,1,-1]
    where (xbdi == 0) ssq = max(0, ssq - xbdi)
    print *, ssq
    if(any(ssq /= [1, 10, 1, 0])) error stop
    end subroutine
end program