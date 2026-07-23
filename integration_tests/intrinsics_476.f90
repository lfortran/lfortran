program intrinsics_476
    ! scale(x, i) with a negative i. The runtime implementation used to compute
    ! 2**i in integer arithmetic, which truncates to 0 for every i < 0, so
    ! scale(x, -1) wrongly returned 0.0 instead of x/2.
    !
    ! The arguments are deliberately variables, not parameters: a constant
    ! expression is folded at compile time by a separate code path that was
    ! never affected by the bug.
    implicit none
    real(8) :: x8, r8
    real(4) :: x4, r4
    integer :: k
    integer :: arr_i(3)
    real(8) :: arr8(3)

    x8 = 1.0_8
    k = -1
    r8 = scale(x8, k)
    print *, r8
    ! 0.5 is exactly representable in binary FP, so require an exact match
    if (r8 /= 0.5_8) error stop

    ! larger negative exponent
    k = -3
    r8 = scale(x8, k)
    print *, r8
    if (r8 /= 0.125_8) error stop

    ! negative exponent on a negative value
    x8 = -6.0_8
    k = -2
    r8 = scale(x8, k)
    print *, r8
    if (r8 /= -1.5_8) error stop

    ! real(4) must behave the same way
    x4 = 1.0_4
    k = -2
    r4 = scale(x4, k)
    print *, r4
    if (r4 /= 0.25_4) error stop

    ! positive exponent still works (guards against over-correcting the fix)
    x8 = 3.0_8
    k = 4
    r8 = scale(x8, k)
    print *, r8
    if (r8 /= 48.0_8) error stop

    ! i == 0 is the identity
    x8 = 7.25_8
    k = 0
    r8 = scale(x8, k)
    print *, r8
    if (r8 /= 7.25_8) error stop

    ! elemental form with a mix of negative, zero and positive exponents
    arr8 = [1.0_8, 8.0_8, 5.0_8]
    arr_i = [-1, -3, 0]
    print *, scale(arr8, arr_i)
    if (any(scale(arr8, arr_i) /= [0.5_8, 1.0_8, 5.0_8])) error stop

    print *, "PASS"
end program
