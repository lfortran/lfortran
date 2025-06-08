program intrinsics_327
    real :: A(2,3) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
    real, allocatable :: B(:, :)
    allocate(B(3, 2))
    call wrapper_transpose(A, B)
    print *, B(1, 1)
    if (B(1, 1) /= 1.0) error stop
    print *, B(1, 2)
    if (B(1, 2) /= 2.0) error stop
    print *, B(2, 1)
    if (B(2, 1) /= 3.0) error stop
    print *, B(2, 2)
    if (B(2, 2) /= 4.0) error stop
    print *, B(3, 1)
    if (B(3, 1) /= 5.0) error stop
    print *, B(3, 2)
    if (B(3, 2) /= 6.0) error stop

    contains

    subroutine wrapper_transpose(M, N)
        real, intent(in) :: M(:, :)
        real, intent(out) :: N(:, :)
        N = transpose(M)
    end subroutine
end program
