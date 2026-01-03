! Test that `BinOp` on arrays inside `where` works and result in correct `BinOp` after indexing.  
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
        
        ssq = [100,0,100,3]
        where (xbdi == 0) ssq = max(0, 2 - ssq)
        print *, ssq
        if(any(ssq /= [100, 2, 100, 0])) error stop
        
        ssq = [100,0,100,3]
        where (xbdi == 0) ssq = max(0,ssq - 2)
        print *, ssq
        if(any(ssq /= [100, 0, 100, 1])) error stop
    end subroutine
end program