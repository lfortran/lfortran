program arrays_reshape_22

    implicit none
    call f1()
    
contains
    
    subroutine f1()
        real, allocatable :: amat(:, :)
        call f2(amat)

        ! call multiple times to make sure there is no memory corruption
        print *, ubound(amat, 1), ubound(amat, 2)
        print *, ubound(amat, 1), ubound(amat, 2)
        print *, ubound(amat, 1), ubound(amat, 2)
        print *, ubound(amat, 1), ubound(amat, 2)
        if(ubound(amat, 1) /= 2 .or. ubound(amat, 2) /= 2 ) error stop
        if(ubound(amat, 1) /= 2 .or. ubound(amat, 2) /= 2 ) error stop
        if(ubound(amat, 1) /= 2 .or. ubound(amat, 2) /= 2 ) error stop
        if(ubound(amat, 1) /= 2 .or. ubound(amat, 2) /= 2 ) error stop
    end subroutine 
    
  
    subroutine f2(amat)
        real, intent(out), allocatable :: amat(:, :)
        allocate(amat(2,2))
        amat = 1.0
        print *, amat
        amat = reshape(source=amat, shape=[2, 2])
    end subroutine 
    
end program 