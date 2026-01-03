program arrays_67
    integer :: A(4) = [1,2,3,4]
    call temp(A)
    print *, A
    if (A(1) /= 5) error stop
    if (A(2) /= 5) error stop
contains
    subroutine temp(A)
        integer, intent(inout) :: A(:)
        A([1,2]) = [5,5]
    end subroutine
end program