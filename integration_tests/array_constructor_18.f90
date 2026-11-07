program array_constructor_18
    ! Array constructor whose type-spec differs from the type of the
    ! implied-do values: the values are converted and the implied-do loops
    ! are still counted as loops.
    implicit none
    integer, parameter :: n = 3
    integer :: i, j, m
    real :: a(3), b(6), c(4), t(5)
    real(8) :: d(3)
    complex :: z(3)

    m = 3

    a = [real :: (i * i, i = 1, n)]
    if (any(abs(a - [1.0, 4.0, 9.0]) > 1e-6)) error stop 1

    t = [real :: (i, i = 1, 4), 5]
    if (any(abs(t - [1.0, 2.0, 3.0, 4.0, 5.0]) > 1e-6)) error stop 2

    b = [real :: (i, i * 10, i = 1, 3)]
    if (any(abs(b - [1.0, 10.0, 2.0, 20.0, 3.0, 30.0]) > 1e-6)) error stop 3

    ! nested implied-do
    c = [real :: ((i + j, j = 1, 2), i = 1, 2)]
    if (any(abs(c - [2.0, 3.0, 3.0, 4.0]) > 1e-6)) error stop 4

    ! run-time bound
    d = [real(8) :: (i, i = 1, m)]
    if (any(abs(d - [1.0d0, 2.0d0, 3.0d0]) > 1d-12)) error stop 5

    z = [complex :: (i, i = 1, 3)]
    if (any(abs(z - [(cmplx(real(i), 0.0), i = 1, 3)]) > 1e-6)) error stop 6

    if (size([real :: (i, i = 1, 4)]) /= 4) error stop 7
    if (size([real :: (i, i = 3, 1, -1)]) /= 3) error stop 8
    if (size([real :: (i, i = 1, 0)]) /= 0) error stop 9
    if (size([real :: (i, i = 1, 0), 1]) /= 1) error stop 10
    if (size([real :: (i, i = 1, 3), (j, j = 7, 8)]) /= 5) error stop 11
end program
