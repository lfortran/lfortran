! A specific intrinsic function with the INTRINSIC attribute in a module is
! an interface, also after another procedure declaration statement whose
! interface is a module procedure defined later
module procedure_54_m
    implicit none
    intrinsic :: sin, dsin
    procedure(later) :: procedure_54_f
    procedure(sin) :: procedure_54_g
    procedure(dsin) :: procedure_54_h
contains
    real function later(x)
        real, intent(in) :: x
        later = x
    end function
end module

real function procedure_54_f(x)
    real, intent(in) :: x
    procedure_54_f = 2*x
end function

elemental real function procedure_54_g(x)
    real, intent(in) :: x
    procedure_54_g = 3*x
end function

elemental double precision function procedure_54_h(x)
    double precision, intent(in) :: x
    procedure_54_h = 4*x
end function

program procedure_54
    use procedure_54_m, only: procedure_54_g, procedure_54_h
    implicit none
    real :: y(2)
    double precision :: d
    y = procedure_54_g([1.0, 2.0])
    print *, y
    if (any(abs(y - [3.0, 6.0]) > 1e-6)) error stop
    d = procedure_54_h(0.5d0)
    print *, d
    if (abs(d - 2.0d0) > 1d-12) error stop
end program
