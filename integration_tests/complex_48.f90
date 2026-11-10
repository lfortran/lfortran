program complex_48
    implicit none
    complex, pointer :: zp
    complex, allocatable :: za
    complex, target :: zt
    zt = (1.0, 2.0)
    zp => zt
    allocate(za)
    za = (3.0, 4.0)
    associate (p => zp%im)
        p = 9.0
    end associate
    call set(za%re)
    call set(zp%re)
    print *, zt, za
    if (zt /= (5.0, 9.0) .or. za /= (5.0, 4.0)) error stop
contains
    subroutine set(r)
        real, intent(out) :: r
        r = 5.0
    end subroutine
end program
