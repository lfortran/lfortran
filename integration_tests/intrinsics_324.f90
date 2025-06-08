program intrinsics_324
    integer, allocatable :: A(:) 
    integer :: B(2) = [2,2]
    allocate(A(2))
    A = [1,2]
    A = max(A, B)
    print *, A
    if (any(A /= 2)) error stop
end program