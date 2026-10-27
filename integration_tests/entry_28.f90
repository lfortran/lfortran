! A statement function that calls an external dummy procedure of its host
! must work when the host is entered through an ENTRY statement.
subroutine entry_28_s(f, x, r)
    implicit none
    real, external :: f
    real :: x, r, sf, t
    sf(t) = f(t) + 1.0
    r = sf(x)
    return
    entry entry_28_e(f, x, r)
    r = sf(x) * 2.0
end subroutine

real function entry_28_g(y)
    implicit none
    real :: y
    entry_28_g = y + 1.0
end function

program entry_28
    implicit none
    real, external :: entry_28_g
    real :: r
    call entry_28_e(entry_28_g, 1.0, r)
    print *, r
    if (abs(r - 6.0) > 1e-6) error stop 1
    call entry_28_s(entry_28_g, 1.0, r)
    print *, r
    if (abs(r - 3.0) > 1e-6) error stop 2
    call entry_28_e(entry_28_g, 0.5, r)
    print *, r
    if (abs(r - 5.0) > 1e-6) error stop 3
end program
