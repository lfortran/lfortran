program intrinsics_348
    integer :: A(2, 2) 
    call temp(A)
contains
    subroutine temp(A)
        integer, intent(inout) :: A(:, :)
        integer :: X(4) 
        X = [1,2,3,4]
        print *, reshape(X, shape(A))
    end subroutine
end program