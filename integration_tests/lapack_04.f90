! MRE from LAPACK slals0.f: integer array index into real array
! causes type check assertion: check_equal_type(i32[:], r32[:])
! Pattern: arr(idx(i)) passed to implicit-interface subroutine
subroutine test
    integer :: idx(2)
    real :: arr(4)

    idx(1) = 1
    arr = 1.0

    call sub(arr(idx(1)))
end subroutine test

subroutine sub(x)
    real :: x(*)
    if (x(1) /= 1.0) stop 1
end subroutine sub

program lapack_04
    call test
    print *, 'PASS'
end program lapack_04
