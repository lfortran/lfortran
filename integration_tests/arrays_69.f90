program arrays_69
    integer :: A(4) = [1,2,3,4]
    A([1,2]) = A([2,1])
    print *, A
    if (A(1) /= 2) error stop
    if (A(2) /= 1) error stop
end program