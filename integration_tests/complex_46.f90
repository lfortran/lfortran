module complex_46_mod
    implicit none
contains
    subroutine set_im(c, v)
        complex, intent(inout) :: c(:)
        real, intent(in) :: v
        c(2:)%im = v
    end subroutine

    elemental subroutine set_re(c, v)
        complex, intent(inout) :: c
        real, intent(in) :: v
        c%re = v
    end subroutine

    pure function swapped(c) result(r)
        complex, intent(in) :: c
        complex :: r
        r%re = c%im
        r%im = c%re
    end function
end module

program complex_46
    use complex_46_mod
    implicit none
    complex :: z, za(4), zb(4)
    complex, pointer :: zp, zpa(:)
    complex, allocatable :: zs
    complex, target :: zt, zta(3)
    logical :: m(4)
    integer :: i

    z = (1.0, 1.0)
    z%re = hz()
    if (z /= (3.0, 4.0)) error stop 1

    za = (1.0, 1.0)
    m = [.true., .false., .true., .false.]
    where (m) za%im = 7.0
    if (any(za /= [(1.0, 7.0), (1.0, 1.0), (1.0, 7.0), (1.0, 1.0)])) error stop 2

    forall (i = 1:4) za(i)%re = real(i)
    if (any(za%re /= [1.0, 2.0, 3.0, 4.0])) error stop 3

    do concurrent (i = 1:4)
        zb(i)%im = real(10 * i)
        zb(i)%re = 0.5
    end do
    if (any(zb /= [(0.5, 10.0), (0.5, 20.0), (0.5, 30.0), (0.5, 40.0)])) error stop 4

    za%im = za(4:1:-1)%re
    if (any(za%im /= [4.0, 3.0, 2.0, 1.0])) error stop 5

    zt = (1.0, 2.0)
    zp => zt
    zp%im = 9.0
    if (zt /= (1.0, 9.0)) error stop 6

    zta = (0.0, 0.0)
    zpa => zta(1:3:2)
    zpa%re = [1.0, 2.0]
    if (any(zta /= [(1.0, 0.0), (0.0, 0.0), (2.0, 0.0)])) error stop 7

    allocate(zs)
    zs = (1.0, 1.0)
    zs%im = -1.0
    if (zs /= (1.0, -1.0)) error stop 8

    zb = (0.0, 0.0)
    call set_im(zb, 3.0)
    if (any(zb /= [(0.0, 0.0), (0.0, 3.0), (0.0, 3.0), (0.0, 3.0)])) error stop 9
    call set_re(zb, 2.0)
    if (any(zb%re /= 2.0)) error stop 10

    z = swapped((1.0, 2.0))
    if (z /= (2.0, 1.0)) error stop 11

    associate (p => z%im)
        p = 5.0
    end associate
    if (z /= (2.0, 5.0)) error stop 12
    print *, "ok"
contains
    real function hz()
        z%im = 4.0
        hz = 3.0
    end function
end program
