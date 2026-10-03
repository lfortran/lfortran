! Statement functions that reference a dummy procedure of their host: in a
! subroutine (also in a DO WHILE condition) and in a function, together with
! direct references to the dummy.
subroutine ii92_drive(f, r)
    implicit none
    real, external :: f
    real, intent(out) :: r
    real :: sf, y
    integer :: it
    sf(y) = f(y) * 2.0
    r = f(1.0)
    it = 0
    do while (sf(real(it)) < 6.0)
        it = it + 1
    end do
    r = r + sf(3.0) + real(it)
end subroutine

real function ii92_apply(f, x)
    implicit none
    real, external :: f
    real, intent(in) :: x
    real :: sf, y
    sf(y) = f(y) + f(y)
    ii92_apply = sf(x) + f(x)
end function

program implicit_interface_92
    implicit none
    real, external :: ii92_h, ii92_apply
    real :: r
    call ii92_drive(ii92_h, r)
    if (abs(r - 10.0) > 1e-5) error stop 1
    r = ii92_apply(ii92_h, 2.0)
    if (abs(r - 6.0) > 1e-5) error stop 2
    print *, r
end program

real function ii92_h(x)
    real, intent(in) :: x
    ii92_h = x
end function
