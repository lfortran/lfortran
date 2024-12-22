program arrays_70
    integer :: A(4) = [1,2,3,4]
    call temp(A)
    print *, A
    if (A(1) /= 2) error stop
    if (A(2) /= 1) error stop
contains
    subroutine temp(A)
        integer, intent(inout) :: A(:)
        A([1,2]) = A([2,1])
    end subroutine
end program