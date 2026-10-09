! A namelist group object may be declared after the NAMELIST statement when
! the implicit typing rules give it a type; the later declaration confirms it.
module namelist_40_mod
    implicit none
    integer :: shift = 3
end module

subroutine namelist_40_sub(a)
    namelist /namdyn/ x, y, n, a
    real :: x
    real :: y(3)
    integer :: n
    real :: a
    integer :: u
    x = 1.5
    y = [1.0, 2.0, 3.0]
    n = 7
    open(newunit=u, status="scratch")
    write(u, nml=namdyn)
    x = 0
    y = 0
    n = 0
    a = 0
    rewind(u)
    read(u, nml=namdyn)
    close(u)
    if (abs(x - 1.5) > 1e-6) error stop
    if (any(abs(y - [1.0, 2.0, 3.0]) > 1e-6)) error stop
    if (n /= 7) error stop
    if (abs(a - 2.5) > 1e-6) error stop
end subroutine

subroutine namelist_40_use(k)
    use namelist_40_mod
    integer, intent(out) :: k
    k = shift
end subroutine

program namelist_40
    implicit none
    real :: a
    integer :: k
    a = 2.5
    call namelist_40_sub(a)
    if (abs(a - 2.5) > 1e-6) error stop
    call namelist_40_use(k)
    if (k /= 3) error stop
    print *, "ok"
end program
