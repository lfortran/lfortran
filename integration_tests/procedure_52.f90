elemental real function procedure_52_twice(x)
    real, intent(in) :: x
    procedure_52_twice = 2*x
end function

elemental double precision function procedure_52_half(x)
    double precision, intent(in) :: x
    procedure_52_half = x/2
end function

elemental real function procedure_52_diff(y, x)
    real, intent(in) :: y, x
    procedure_52_diff = y - x
end function

subroutine procedure_52_check()
    implicit none
    ! A specific intrinsic is an interface even without the INTRINSIC statement
    procedure(sqrt) :: procedure_52_twice
    real :: b
    b = procedure_52_twice(-1.5)
    if (abs(b + 3.0) > 1e-6) error stop
end subroutine

program procedure_52
    implicit none
    intrinsic :: sin, dcos, atan2
    procedure(sin) :: procedure_52_twice
    procedure(dcos) :: procedure_52_half
    procedure(atan2) :: procedure_52_diff
    real :: y
    double precision :: d
    y = procedure_52_twice(0.5)
    print *, y
    if (abs(y - 1.0) > 1e-6) error stop
    d = procedure_52_half(3.0d0)
    print *, d
    if (abs(d - 1.5d0) > 1d-12) error stop
    y = procedure_52_diff(x=1.0, y=5.0)
    print *, y
    if (abs(y - 4.0) > 1e-6) error stop
    call procedure_52_check()
end program
