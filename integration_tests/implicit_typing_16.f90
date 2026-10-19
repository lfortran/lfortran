! An external procedure is a global entity, which host association does not
! make accessible: an undeclared name used as a variable in another scoping
! unit is a local, implicitly typed variable, even if an external procedure of
! that name is defined in the same file, before or after that scoping unit.
! A scoping unit that references the name as a procedure accesses it.
subroutine dist()
    common /counter/ ncalls
    ncalls = ncalls + 1
end subroutine

subroutine use_dist(y)
    real :: y
    dist = 1.0
    y = dist + 1.0
end subroutine

real function area()
    area = 2.0
end function

subroutine use_area(y)
    real :: y
    area = 3.0
    y = area
end subroutine

subroutine use_kount(n)
    integer :: n
    kount = 7
    n = kount
end subroutine

subroutine kount()
end subroutine

subroutine point_to_dist()
    interface
        subroutine iface()
        end subroutine
    end interface
    procedure(iface), pointer :: p
    p => dist
    call p()
    call dist()
end subroutine

program implicit_typing_16
    real :: y
    integer :: n
    common /counter/ ncalls
    ncalls = 0
    call use_dist(y)
    if (abs(y - 2.0) > 1e-6) error stop
    call use_area(y)
    if (abs(y - 3.0) > 1e-6) error stop
    if (abs(area() - 2.0) > 1e-6) error stop
    call use_kount(n)
    if (n /= 7) error stop
    call kount()
    call point_to_dist()
    if (ncalls /= 2) error stop
    print *, y, n, ncalls
end program
