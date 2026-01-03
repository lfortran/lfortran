program test_arrays
    implicit none
    real :: a(10, 20, 30)
    call pass_array(a)
    print *, "Test passed"
contains
    subroutine pass_array(arr)
        real :: arr(10, 20, *)
        call recv_array(arr(1:10, 1:20, 1))
    end subroutine pass_array
    
    subroutine recv_array(x)
        real :: x(:, :)
        integer :: ub1, ub2
        ub1 = ubound(x, 1)
        ub2 = ubound(x, 2)
        if (ub1 /= size(x,1)) stop "ubound dim 1 failed"
        if (ub2 /= size(x,2)) stop "ubound dim 2 failed"
    end subroutine recv_array
end program test_arrays