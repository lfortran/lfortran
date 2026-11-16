! A vector subscript that is a column of a fixed-size array, in a mixed
! complex and real expression: the bounds of the column section are taken
! from its extent.
program vector_subscript_05
    implicit none
    real(8) :: r(4), s
    complex(8), allocatable :: z(:, :)
    integer :: p(4, 3), m
    r = [1d0, 2d0, 3d0, 4d0]
    allocate (z(4, 2))
    z = (1d0, 0d0)
    p(:, 1) = [4, 3, 2, 1]
    p(:, 2:3) = 1
    m = 1
    s = sum(abs(z(:, 2) - r(p(:, m))))
    print *, s
    if (abs(s - 6d0) > 1d-14) error stop
end program
