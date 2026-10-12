! MIN and MAX with array arguments of different kinds (a non-standard
! extension): the arguments are converted to the largest kind.
subroutine clip(a, b)
    implicit none
    real(8) :: a(2)
    real(4) :: b(2)
    b(:) = min(a(:), b(:))
end subroutine

program min_03
    implicit none
    interface
        subroutine clip(a, b)
            real(8) :: a(2)
            real(4) :: b(2)
        end subroutine
    end interface
    real(8) :: a(2), d
    real(4) :: b(2)
    integer(4) :: i(3)
    integer(8) :: j(3), k

    a = [1.5d0, -2.5d0]
    b = [3.0, -4.0]
    call clip(a, b)
    if (abs(b(1) - 1.5) > 1e-6) error stop
    if (abs(b(2) + 4.0) > 1e-6) error stop

    b = [3.0, -4.0]
    if (kind(min(a, b)) /= 8) error stop
    if (kind(max(b, a)) /= 8) error stop
    if (any(abs(min(b, a) - [1.5d0, -4.0d0]) > 1d-12)) error stop
    if (any(abs(max(a, b) - [3.0d0, -2.5d0]) > 1d-12)) error stop

    d = 0.1d0
    b = [0.25, -0.25]
    if (kind(min(b, d)) /= 8) error stop
    if (any(abs(min(b, d) - [0.1d0, -0.25d0]) > 1d-12)) error stop
    if (any(abs(max(d, b) - [0.25d0, 0.1d0]) > 1d-12)) error stop

    i = [1, 5, -7]
    j = [3_8, 2_8, -9_8]
    k = 4_8
    if (kind(min(i, j)) /= 8) error stop
    if (any(min(i, j) /= [1_8, 2_8, -9_8])) error stop
    if (any(max(j, i) /= [3_8, 5_8, -7_8])) error stop
    if (any(max(i, k, j) /= [4_8, 5_8, 4_8])) error stop
    print *, min(a, b), max(i, j)
end program
