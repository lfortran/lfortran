program arrays_reshape_58
    ! reshape of an automatic array whose size depends on an assumed-shape
    ! dummy, assigned back to that dummy (#5663)
    integer :: A(2,2) = reshape([1,2,3,4],[2,2])
    A = 0
    call temp(A)
    print *, A
    if (any(A(:,1) /= [1, 2])) error stop
    if (any(A(:,2) /= [3, 4])) error stop
contains
    subroutine temp(A)
        integer, intent(inout) :: A(:,:)
        integer :: X(size(A,1)*size(A,2))
        X = [1,2,3,4]
        A = reshape(X, shape(A))
        print *, X
        if (size(X) /= 4) error stop
        if (any(X /= [1, 2, 3, 4])) error stop
    end subroutine
end program
