program real10_transfer_fold_01
    ! transfer of real(10) named constants, scalar and array, gives their
    ! x87 extended storage. Their bytes depend on the target, so they are
    ! not folded from the value of the constants.
    implicit none
    real(10), parameter :: c(2) = [1.5_10, -2.25_10], cs = 1.5_10
    integer(8) :: g(4), h(2)
    integer(1) :: b(32)
    real(10) :: f(2), s

    f = transfer(transfer(c, [0_8]), f)
    if (any(f /= c)) error stop
    f = transfer(transfer(c, [0_1]), f)
    if (any(f /= c)) error stop
    s = transfer(transfer(cs, [0_8]), s)
    if (s /= cs) error stop
    s = transfer(transfer(cs, [0_1]), s)
    if (s /= cs) error stop

    if (size(transfer(c, [0_8])) /= 4) error stop
    if (size(transfer(c, [0_1])) /= 32) error stop
    if (size(transfer(cs, [0_8])) /= 2) error stop

    ! The 6 bytes above the 80-bit value are padding
    g = transfer(c, [0_8])
    if (g(1) /= -4611686018427387904_8) error stop
    if (iand(g(2), 65535_8) /= 16383) error stop
    if (g(3) /= -8070450532247928832_8) error stop
    if (iand(g(4), 65535_8) /= 49152) error stop
    h = transfer(cs, [0_8])
    if (h(1) /= -4611686018427387904_8) error stop
    if (iand(h(2), 65535_8) /= 16383) error stop
    b = transfer(c, [0_1])
    if (any(b(1:7) /= 0) .or. b(8) /= -64 .or. b(9) /= -1 .or. b(10) /= 63) error stop
    if (any(b(17:23) /= 0) .or. b(24) /= -112 .or. b(25) /= 0 .or. b(26) /= -64) error stop
    print *, f, s, g(1), h(1)
end program
