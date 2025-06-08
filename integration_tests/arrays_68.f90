program arrays_68
    integer :: A(4) = [1,2,3,4]
    A([1,2]) = [5,5]
    print *, A
    if (A(1) /= 5) error stop
    if (A(2) /= 5) error stop
end program