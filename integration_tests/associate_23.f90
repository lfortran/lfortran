program pointer_lhs_section_bug
    implicit none

    real, target :: arr(6)
    real, pointer :: mat(:), mat1(:)
    integer :: n

    n = 6
    arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

    mat(1:n-3) => arr
    print *, "Test 1: mat(1:n-3) => arr"
    print *, "  mat:", mat
    print *, "  lbound:", lbound(mat)
    print *, "  ubound:", ubound(mat)

    if (any(mat /= [1.0, 2.0, 3.0])) error stop
    if (any(lbound(mat) /= 1)) error stop
    if (any(ubound(mat) /= 3)) error stop

    mat1(1+2:n+2) => arr
    print *, "Test 2: mat1(1+2:n+2) => arr"
    print *, "  mat1:", mat1
    print *, "  lbound:", lbound(mat1)
    print *, "  ubound:", ubound(mat1)
    if (any(mat1 /= [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])) error stop
    if (any(lbound(mat1) /= 3)) error stop
    if (any(ubound(mat1) /= 8)) error stop

end program pointer_lhs_section_bug