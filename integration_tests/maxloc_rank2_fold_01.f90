program maxloc_rank2_fold_01
    ! maxloc/minloc of rank-2 (and rank-3) constant arrays folded in
    ! PARAMETER initializers: no dim, dim=, mask=, back= and kind=
    implicit none
    integer, parameter :: m(2,3) = reshape([1, 4, 3, 2, 6, 5], [2,3])
    integer, parameter :: m2(2,2) = reshape([4, 1, 1, 4], [2,2])
    real(8), parameter :: r(2,3) = reshape([1.5d0, -2.0d0, 7.0d0, 7.0d0, &
        0.0d0, 3.0d0], [2,3])
    integer, parameter :: t(2,2,2) = reshape([3, 8, 1, 5, 2, 7, 6, 4], [2,2,2])

    integer, parameter :: a1(2) = maxloc(m)
    integer, parameter :: a2(2) = minloc(m)
    integer, parameter :: a3(3) = maxloc(m, dim=1)
    integer, parameter :: a4(2) = maxloc(m, dim=2)
    integer, parameter :: a5(3) = minloc(m, dim=1)
    integer, parameter :: a6(2) = minloc(m, dim=2)

    integer, parameter :: b1(2) = maxloc(m, mask=m < 5)
    integer, parameter :: b2(2) = minloc(m, mask=m > 1)
    integer, parameter :: b3(3) = maxloc(m, dim=1, mask=m /= 4)
    integer, parameter :: b4(2) = maxloc(m, mask=m > 100)
    integer, parameter :: b5(3) = minloc(m, dim=1, mask=m > 100)
    integer, parameter :: b6(2) = maxloc(m, mask=.true.)
    integer, parameter :: b7(2) = minloc(m, mask=.false.)
    integer, parameter :: b8(2) = minloc(m, dim=2, mask=m > 1)

    integer, parameter :: c1(2) = maxloc(m2, back=.true.)
    integer, parameter :: c2(2) = minloc(m2, back=.true.)
    integer, parameter :: c3(2) = maxloc(m2)
    integer, parameter :: c4(2) = minloc(m2)
    integer, parameter :: c5(2) = maxloc(m2, dim=1, back=.true.)
    integer, parameter :: c6(2) = minloc(m2, dim=2, back=.true.)

    integer(8), parameter :: d1(2) = maxloc(m, kind=8)
    integer(8), parameter :: d2(3) = minloc(m, dim=1, kind=8)
    integer(8), parameter :: d3(2) = maxloc(r, kind=8)

    integer, parameter :: e1(2) = maxloc(r)
    integer, parameter :: e2(2) = minloc(r)
    integer, parameter :: e3(2) = maxloc(r, back=.true.)
    integer, parameter :: e4(3) = maxloc(r, dim=1)
    integer, parameter :: e5(3) = maxloc(r, dim=1, back=.true.)
    integer, parameter :: e6(2) = minloc(r, dim=2, mask=r > 0.0d0)

    integer, parameter :: f1(3) = maxloc(t)
    integer, parameter :: f2(2,2) = maxloc(t, dim=3)
    integer, parameter :: f3(2,2) = minloc(t, dim=1)

    ! Results used in other constant expressions
    integer, parameter :: g1 = b1(1) + 10
    integer, parameter :: g2 = sum(maxloc(m, dim=1)) * 100 + product(minloc(r))
    integer, parameter :: g3(2) = shape(maxloc(t, dim=2))
    integer :: z(b1(2) + 2)

    logical :: lmask(2,3)
    integer :: h(2)

    if (any(a1 /= [1, 3])) error stop 1
    if (any(a2 /= [1, 1])) error stop 2
    if (any(a3 /= [2, 1, 1])) error stop 3
    if (any(a4 /= [3, 3])) error stop 4
    if (any(a5 /= [1, 2, 2])) error stop 5
    if (any(a6 /= [1, 2])) error stop 6

    if (any(b1 /= [2, 1])) error stop 7
    if (any(b2 /= [2, 2])) error stop 8
    if (any(b3 /= [1, 1, 1])) error stop 9
    if (any(b4 /= [0, 0])) error stop 10
    if (any(b5 /= [0, 0, 0])) error stop 11
    if (any(b6 /= [1, 3])) error stop 12
    if (any(b7 /= [0, 0])) error stop 13
    if (any(b8 /= [2, 2])) error stop 14

    if (any(c1 /= [2, 2])) error stop 15
    if (any(c2 /= [1, 2])) error stop 16
    if (any(c3 /= [1, 1])) error stop 17
    if (any(c4 /= [2, 1])) error stop 18
    if (any(c5 /= [1, 2])) error stop 19
    if (any(c6 /= [2, 1])) error stop 20

    if (any(d1 /= [1_8, 3_8])) error stop 21
    if (any(d2 /= [1_8, 2_8, 2_8])) error stop 22
    if (any(d3 /= [1_8, 2_8])) error stop 23
    if (kind(maxloc(m, kind=8)) /= 8) error stop 24

    if (any(e1 /= [1, 2])) error stop 25
    if (any(e2 /= [2, 1])) error stop 26
    if (any(e3 /= [2, 2])) error stop 27
    if (any(e4 /= [1, 1, 2])) error stop 28
    if (any(e5 /= [1, 2, 2])) error stop 29
    if (any(e6 /= [1, 3])) error stop 30

    if (any(f1 /= [2, 1, 1])) error stop 31
    if (any(f2 /= reshape([1, 1, 2, 1], [2,2]))) error stop 32
    if (any(f3 /= reshape([1, 1, 1, 2], [2,2]))) error stop 33

    if (g1 /= 12) error stop 34
    if (g2 /= 402) error stop 35
    if (any(g3 /= [2, 2])) error stop 36
    if (size(z) /= 3) error stop 37

    ! A mask known only at run time is not folded
    lmask = reshape([.true., .false., .true., .true., .false., .true.], [2,3])
    h = maxloc(m, mask=lmask)
    if (any(h /= [2, 3])) error stop 38

    print *, a1, a3, b1, c1, d1, e1, f2
end program
