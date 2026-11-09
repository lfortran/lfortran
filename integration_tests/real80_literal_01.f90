program real80_literal_01
    ! real(10) constants (literals and folded constant expressions) must have
    ! the same value as the equal real(10) numbers computed at run time from
    ! integer variables (#17031). All values are exactly representable.
    implicit none
    real(10), parameter :: p = 2.5_10
    real(10), parameter :: q = -p
    real(10), parameter :: r = 2 * p + 0.25_10
    integer :: i, j, k
    real(10) :: a, b, c, x, y, z
    real(8) :: d

    i = 3
    j = 2
    k = 4
    a = i
    b = j
    c = k
    d = 1.5d0

    x = 1.5_10
    if (x /= a/b) error stop 1
    if (x /= d) error stop 2
    if (.not. (x == 1.5_10)) error stop 3
    y = 1.5d0
    if (y /= a/b) error stop 4
    z = 3
    if (z /= a) error stop 5
    if (p /= (a + b)/b) error stop 6
    if (q /= -(a + b)/b) error stop 7
    if (r /= a + b + 1/c) error stop 8
    if (0.25_10 - 1 /= -a/c) error stop 9
    if (1024.0_10 /= c**5) error stop 10
    if (2.0_10**10 /= c**5) error stop 11
    if (x + 1 /= p) error stop 12
    if (x * 2.0_10 /= a) error stop 13
    if (a / 2 /= x) error stop 14
    if (-x /= -a/b) error stop 15
    d = x
    if (d /= 1.5d0) error stop 16
    print *, "ok"
end program real80_literal_01
