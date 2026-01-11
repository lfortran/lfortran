program array_bound_5
implicit none
real :: arr(10, 20)

call test_assumed_size_bounds(arr)

contains

subroutine test_assumed_size_bounds(a)
    real :: a(10, *)
    ! lbound/ubound on non-last dimensions are valid for assumed-size arrays
    if (lbound(a, 1) /= 1) error stop
    if (ubound(a, 1) /= 10) error stop
    if (lbound(a, 2) /= 1) error stop
    ! ubound(a, 2) would be an error - last dim is assumed-size
    print *, "OK: assumed-size array bounds"
end subroutine

end program
