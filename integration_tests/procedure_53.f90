! Specific intrinsic functions whose result type differs from the type of
! their arguments, as interfaces of external procedures
elemental integer function procedure_53_round(a)
    real, intent(in) :: a
    procedure_53_round = int(a) + 1
end function

elemental real function procedure_53_norm1(a)
    complex, intent(in) :: a
    procedure_53_norm1 = abs(real(a)) + abs(aimag(a))
end function

elemental double precision function procedure_53_prod(x, y)
    real, intent(in) :: x, y
    procedure_53_prod = dble(x) * dble(y) + 1
end function

elemental integer function procedure_53_dround(a)
    double precision, intent(in) :: a
    procedure_53_dround = int(a) + 2
end function

elemental real function procedure_53_re(z)
    complex, intent(in) :: z
    procedure_53_re = real(z)
end function

program procedure_53
    implicit none
    intrinsic :: nint, cabs, dprod, idnint, aimag
    procedure(nint) :: procedure_53_round
    procedure(cabs) :: procedure_53_norm1
    procedure(dprod) :: procedure_53_prod
    procedure(idnint) :: procedure_53_dround
    procedure(aimag) :: procedure_53_re
    integer :: i, iv(2)
    real :: r
    double precision :: d
    i = procedure_53_round(2.2)
    print *, i
    if (i /= 3) error stop
    iv = procedure_53_round([1.5, 4.5])
    print *, iv
    if (any(iv /= [2, 5])) error stop
    r = procedure_53_norm1((3.0, -4.0))
    print *, r
    if (abs(r - 7.0) > 1e-6) error stop
    d = procedure_53_prod(1.5, 2.0)
    print *, d
    if (abs(d - 4.0d0) > 1d-12) error stop
    i = procedure_53_dround(2.6d0)
    print *, i
    if (i /= 4) error stop
    r = procedure_53_re((1.0, 2.0))
    print *, r
    if (abs(r - 1.0) > 1e-6) error stop
end program
