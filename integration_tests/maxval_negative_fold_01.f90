program maxval_negative_fold_01
    ! MAXVAL and MINVAL of constant real arrays folded at compile time start
    ! from the first selected element: all-negative arrays give their largest
    ! element, not tiny, and nothing selected gives -huge (MAXVAL) or +huge
    ! (MINVAL) for every real kind (#16143). Integers stay -huge-1 and huge.
    implicit none
    real, parameter :: p(2) = [-1., -2.]
    real, parameter :: pm = maxval(p, mask=p > 0.)

    real, parameter :: a4(4) = [-3., -1., -2., -5.]
    real, parameter :: b4(2,3) = reshape([-4., -1., -3., -2., -6., -5.], [2, 3])
    real(8), parameter :: a8(4) = [-3._8, -1._8, -2._8, -5._8]
    real(8), parameter :: b8(2,3) = reshape([-4._8, -1._8, -3._8, -2._8, &
        -6._8, -5._8], [2, 3])
    real(16), parameter :: a16(4) = [-3._16, -1._16, -2._16, -5._16]
    real(16), parameter :: b16(2,3) = reshape([-4._16, -1._16, -3._16, &
        -2._16, -6._16, -5._16], [2, 3])
    integer, parameter :: ai(4) = [-3, -1, -2, -5]
    integer, parameter :: bi(2,3) = reshape([-4, -1, -3, -2, -6, -5], [2, 3])
    logical, parameter :: ma(4) = [.true., .false., .true., .false.]
    logical, parameter :: mf(4) = .false.
    logical, parameter :: m2(2,3) = reshape([.true., .false., .false., &
        .false., .true., .true.], [2, 3])

    real, parameter :: x4(9) = [maxval(a4), minval(a4), &
        maxval(a4, mask=ma), minval(a4, mask=ma), &
        maxval(a4, mask=mf), minval(a4, mask=mf), &
        maxval(a4, mask=.false.), minval(a4, mask=.false.), &
        maxval(a4, dim=1)]
    real, parameter :: y4(2) = [maxval(a4, dim=1, mask=ma), &
        maxval(a4, dim=1, mask=mf)]
    real, parameter :: c4_1(3) = maxval(b4, dim=1), c4_2(2) = maxval(b4, dim=2)
    real, parameter :: d4_1(3) = minval(b4, dim=1), d4_2(2) = minval(b4, dim=2)
    real, parameter :: e4_1(3) = maxval(b4, dim=1, mask=m2)
    real, parameter :: e4_2(2) = maxval(b4, dim=2, mask=m2)
    real, parameter :: f4_1(3) = minval(b4, dim=1, mask=m2)
    real, parameter :: g4 = maxval(b4), h4 = maxval(b4, mask=m2)

    real(8), parameter :: x8(9) = [maxval(a8), minval(a8), &
        maxval(a8, mask=ma), minval(a8, mask=ma), &
        maxval(a8, mask=mf), minval(a8, mask=mf), &
        maxval(a8, mask=.false.), minval(a8, mask=.false.), &
        maxval(a8, dim=1)]
    real(8), parameter :: y8(2) = [maxval(a8, dim=1, mask=ma), &
        maxval(a8, dim=1, mask=mf)]
    real(8), parameter :: c8_1(3) = maxval(b8, dim=1), c8_2(2) = maxval(b8, dim=2)
    real(8), parameter :: d8_1(3) = minval(b8, dim=1), d8_2(2) = minval(b8, dim=2)
    real(8), parameter :: e8_1(3) = maxval(b8, dim=1, mask=m2)
    real(8), parameter :: e8_2(2) = maxval(b8, dim=2, mask=m2)
    real(8), parameter :: f8_1(3) = minval(b8, dim=1, mask=m2)
    real(8), parameter :: g8 = maxval(b8), h8 = maxval(b8, mask=m2)

    real(16), parameter :: x16(9) = [maxval(a16), minval(a16), &
        maxval(a16, mask=ma), minval(a16, mask=ma), &
        maxval(a16, mask=mf), minval(a16, mask=mf), &
        maxval(a16, mask=.false.), minval(a16, mask=.false.), &
        maxval(a16, dim=1)]
    real(16), parameter :: s16_1 = maxval(a16, mask=.false.)
    real(16), parameter :: s16_2 = minval(a16, mask=.false.)
    real(16), parameter :: y16(2) = [maxval(a16, dim=1, mask=ma), &
        maxval(a16, dim=1, mask=mf)]
    real(16), parameter :: c16_1(3) = maxval(b16, dim=1)
    real(16), parameter :: c16_2(2) = maxval(b16, dim=2)
    real(16), parameter :: d16_1(3) = minval(b16, dim=1)
    real(16), parameter :: e16_1(3) = maxval(b16, dim=1, mask=m2)
    real(16), parameter :: e16_2(2) = maxval(b16, dim=2, mask=m2)
    real(16), parameter :: g16 = maxval(b16), h16 = maxval(b16, mask=m2)

    integer, parameter :: xi(9) = [maxval(ai), minval(ai), &
        maxval(ai, mask=ma), minval(ai, mask=ma), &
        maxval(ai, mask=mf), minval(ai, mask=mf), &
        maxval(ai, mask=.false.), minval(ai, mask=.false.), &
        maxval(ai, dim=1)]
    integer, parameter :: ci_1(3) = maxval(bi, dim=1), ci_2(2) = maxval(bi, dim=2)
    integer, parameter :: ei_1(3) = maxval(bi, dim=1, mask=m2)
    integer, parameter :: fi_1(3) = minval(bi, dim=1, mask=m2)

    real :: big4
    real(8) :: big8
    real(16) :: big16
    integer :: bigi

    big4 = huge(1.0)
    big8 = huge(1.0_8)
    big16 = huge(1.0_16)
    bigi = huge(1)

    ! The program from the issue
    if (maxval(p) /= -1.) error stop 1
    if (pm /= -big4) error stop 2

    ! real(4) parameter initializers
    if (any(x4 /= [-1., -5., -2., -3., -big4, big4, -big4, big4, -1.])) error stop 10
    if (any(y4 /= [-2., -big4])) error stop 11
    if (any(c4_1 /= [-1., -2., -5.]) .or. any(c4_2 /= [-3., -1.])) error stop 12
    if (any(d4_1 /= [-4., -3., -6.]) .or. any(d4_2 /= [-6., -5.])) error stop 13
    if (any(e4_1 /= [-4., -big4, -5.]) .or. any(e4_2 /= [-4., -5.])) error stop 14
    if (any(f4_1 /= [-4., big4, -6.])) error stop 15
    if (g4 /= -1. .or. h4 /= -4.) error stop 16
    ! real(4) used directly in expressions
    if (maxval(a4) /= -1. .or. minval(a4) /= -5.) error stop 17
    if (maxval(a4, mask=ma) /= -2. .or. minval(a4, mask=ma) /= -3.) error stop 18
    if (maxval(a4, mask=a4 > 0.) /= -big4) error stop 19
    if (minval(a4, mask=a4 > 0.) /= big4) error stop 20
    if (maxval(a4, mask=.false.) /= -big4) error stop 21
    if (maxval(a4, dim=1) /= -1. .or. maxval(a4, 1, a4 < -1.5) /= -2.) error stop 22
    if (any(maxval(b4, dim=1) /= [-1., -2., -5.])) error stop 23
    if (any(maxval(b4, dim=2) /= [-3., -1.])) error stop 24
    if (any(maxval(b4, dim=1, mask=m2) /= [-4., -big4, -5.])) error stop 25
    if (any(maxval(b4, dim=2, mask=m2) /= [-4., -5.])) error stop 26
    if (any(minval(b4, dim=1, mask=m2) /= [-4., big4, -6.])) error stop 27

    ! real(8) parameter initializers
    if (any(x8 /= [-1._8, -5._8, -2._8, -3._8, -big8, big8, -big8, big8, &
        -1._8])) error stop 30
    if (any(y8 /= [-2._8, -big8])) error stop 31
    if (any(c8_1 /= [-1._8, -2._8, -5._8]) .or. any(c8_2 /= [-3._8, -1._8])) error stop 32
    if (any(d8_1 /= [-4._8, -3._8, -6._8]) .or. any(d8_2 /= [-6._8, -5._8])) error stop 33
    if (any(e8_1 /= [-4._8, -big8, -5._8]) .or. any(e8_2 /= [-4._8, -5._8])) error stop 34
    if (any(f8_1 /= [-4._8, big8, -6._8])) error stop 35
    if (g8 /= -1._8 .or. h8 /= -4._8) error stop 36
    ! real(8) used directly in expressions
    if (maxval(a8) /= -1._8 .or. minval(a8) /= -5._8) error stop 37
    if (maxval(a8, mask=ma) /= -2._8 .or. minval(a8, mask=ma) /= -3._8) error stop 38
    if (maxval(a8, mask=a8 > 0._8) /= -big8) error stop 39
    if (minval(a8, mask=a8 > 0._8) /= big8) error stop 40
    if (maxval(a8, mask=.false.) /= -big8) error stop 41
    if (maxval(a8, dim=1) /= -1._8 .or. maxval(a8, 1, a8 < -1.5_8) /= -2._8) error stop 42
    if (any(maxval(b8, dim=1) /= [-1._8, -2._8, -5._8])) error stop 43
    if (any(maxval(b8, dim=2) /= [-3._8, -1._8])) error stop 44
    if (any(maxval(b8, dim=1, mask=m2) /= [-4._8, -big8, -5._8])) error stop 45
    if (any(maxval(b8, dim=2, mask=m2) /= [-4._8, -5._8])) error stop 46
    if (any(minval(b8, dim=1, mask=m2) /= [-4._8, big8, -6._8])) error stop 47

    ! real(16) parameter initializers
    if (any(x16 /= [-1._16, -5._16, -2._16, -3._16, -big16, big16, -big16, &
        big16, -1._16])) error stop 50
    if (s16_1 /= -big16 .or. s16_2 /= big16) error stop 51
    if (any(y16 /= [-2._16, -big16])) error stop 52
    if (any(c16_1 /= [-1._16, -2._16, -5._16])) error stop 53
    if (any(c16_2 /= [-3._16, -1._16])) error stop 54
    if (any(d16_1 /= [-4._16, -3._16, -6._16])) error stop 55
    if (any(e16_1 /= [-4._16, -big16, -5._16])) error stop 56
    if (any(e16_2 /= [-4._16, -5._16])) error stop 57
    if (g16 /= -1._16 .or. h16 /= -4._16) error stop 58
    ! real(16) used directly in expressions
    if (maxval(a16) /= -1._16 .or. minval(a16) /= -5._16) error stop 59
    if (maxval(a16, mask=ma) /= -2._16) error stop 60
    if (maxval(a16, mask=.false.) /= -big16) error stop 61
    if (minval(a16, mask=.false.) /= big16) error stop 62
    if (maxval(a16, mask=a16 > 0._16) /= -big16) error stop 63
    if (any(maxval(b16, dim=1, mask=m2) /= [-4._16, -big16, -5._16])) error stop 64

    ! integer: nothing selected gives -huge-1 and huge
    if (any(xi /= [-1, -5, -2, -3, -bigi - 1, bigi, -bigi - 1, bigi, -1])) error stop 70
    if (any(ci_1 /= [-1, -2, -5]) .or. any(ci_2 /= [-3, -1])) error stop 71
    if (any(ei_1 /= [-4, -bigi - 1, -5])) error stop 72
    if (any(fi_1 /= [-4, bigi, -6])) error stop 73
    if (maxval(ai) /= -1 .or. maxval(ai, mask=.false.) /= -bigi - 1) error stop 74
    if (any(maxval(bi, dim=2, mask=m2) /= [-4, -5])) error stop 75

    print *, "ok"
end program
