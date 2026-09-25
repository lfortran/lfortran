program real128_compare_mixed_01
    ! real(16) compared with, and assigned from, constants of other real kinds
    implicit none
    real(16) :: q, r
    real(8) :: d
    real(4) :: s
    real(16), parameter :: p = 1.0d0
    real(16), parameter :: ps = 0.5
    logical, parameter :: lp = 2.0_16 > 1.0d0

    q = 1.0_16
    d = 2.0d0
    s = 0.25

    if (.not. (q == 1.0d0)) error stop 1
    if (q /= 1.0d0) error stop 2
    if (.not. (q < 2.0d0)) error stop 3
    if (.not. (q > 0.5)) error stop 4
    if (.not. (huge(d) < huge(q))) error stop 5
    if (.not. (tiny(q) < tiny(s))) error stop 6
    if (.not. (1.0d0 == q)) error stop 7
    if (.not. lp) error stop 8
    if (p /= 1.0_16) error stop 9
    if (ps /= 0.5_16) error stop 10

    r = 1.0d0
    if (r /= q) error stop 11
    r = 0.25
    if (r /= 0.25_16) error stop 12
    r = q + 2.0d0
    if (r /= 3.0_16) error stop 13
    r = 1.5d0 * 2.0_16
    if (r /= 3.0_16) error stop 14
    r = 0.1d0
    if (abs(r - 0.1d0) > 0.0_16) error stop 15
    d = 3.0_16
    if (d /= 3.0d0) error stop 16
    print *, "ok"
end program
