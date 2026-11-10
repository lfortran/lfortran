module complex_45_mod
    implicit none
    complex :: z, za(2)
    integer :: ncalls = 0
contains
    real function hr()
        z%re = 4.0
        hr = 3.0
    end function

    real function hi()
        z%im = 5.0
        hi = 6.0
    end function

    function har() result(r)
        real :: r(2)
        za%re = 4.0
        r = 3.0
    end function

    integer function idx()
        ncalls = ncalls + 1
        idx = 2
    end function
end module

program complex_45
    use complex_45_mod
    implicit none
    type :: t
        complex :: c
        complex(8) :: arr(3)
    end type
    type(t) :: x
    complex :: zl, z2(2, 3)
    complex(8) :: zd
    complex, allocatable :: zal(:)
    integer :: i

    z = (1.0, 1.0)
    z%im = hr()
    if (z /= (4.0, 3.0)) error stop 1

    z = (1.0, 1.0)
    z%re = hi()
    if (z /= (6.0, 5.0)) error stop 2

    za = (1.0, 1.0)
    za%im = har()
    if (any(za /= (4.0, 3.0))) error stop 3

    za = (1.0, 1.0)
    za(idx())%im = 2.0
    if (ncalls /= 1) error stop 4
    if (za(2) /= (1.0, 2.0)) error stop 5

    zl = (1.0, 2.0)
    zl%re = 7
    zl%im = zl%re + 1
    if (zl /= (7.0, 8.0)) error stop 6

    zd = (1.0d0, 2.0d0)
    zd%im = 3.5
    if (zd /= (1.0d0, 3.5d0)) error stop 7

    x%c = (1.0, 1.0)
    x%c%re = 9.0
    x%arr = (0.0d0, 0.0d0)
    x%arr(2)%im = 4.0d0
    x%arr%re = [1.0d0, 2.0d0, 3.0d0]
    if (x%c /= (9.0, 1.0)) error stop 8
    if (any(x%arr /= [(1.0d0, 0.0d0), (2.0d0, 4.0d0), (3.0d0, 0.0d0)])) error stop 9

    z2 = (0.0, 0.0)
    z2(:, 2)%im = [1.0, 2.0]
    z2(1, :)%re = 5.0
    do i = 1, 3
        if (z2(1, i)%re /= 5.0) error stop 10
    end do
    if (z2(2, 2) /= (0.0, 2.0)) error stop 11
    if (z2(1, 2) /= (5.0, 1.0)) error stop 12

    allocate(zal(4))
    zal = (1.0, 1.0)
    zal(1:4:2)%im = [8.0, 9.0]
    zal%re = zal%im * 2
    if (any(zal /= [(16.0, 8.0), (2.0, 1.0), (18.0, 9.0), (2.0, 1.0)])) error stop 13
    print *, "ok"
end program
