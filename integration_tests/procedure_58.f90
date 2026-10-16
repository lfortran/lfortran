! A specific intrinsic function is the interface of a procedure declaration
! statement in a module, in its module procedures and in a BLOCK of one,
! without an INTRINSIC statement, unless the name is a procedure of the module
module procedure_58_m
    implicit none
    procedure(sin) :: procedure_58_f
    procedure(dsin) :: procedure_58_g
contains
    real function call_sin(x)
        real, intent(in) :: x
        procedure(sin) :: procedure_58_a
        call_sin = procedure_58_a(x)
    end function

    subroutine call_dsin_cos(x, r)
        double precision, intent(in) :: x
        double precision, intent(out) :: r
        procedure(dsin) :: procedure_58_b
        block
            procedure(cos) :: procedure_58_c
            r = procedure_58_b(x) + procedure_58_c(real(x))
        end block
    end subroutine
end module

module procedure_58_own_m
    implicit none
contains
    real function sqrt(x, y)
        real, intent(in) :: x, y
        sqrt = x + y
    end function

    real function call_own_sqrt(x, y)
        real, intent(in) :: x, y
        procedure(sqrt) :: procedure_58_d
        call_own_sqrt = procedure_58_d(x, y)
    end function
end module

elemental real function procedure_58_f(x)
    real, intent(in) :: x
    procedure_58_f = 2*x
end function

elemental double precision function procedure_58_g(x)
    double precision, intent(in) :: x
    procedure_58_g = 3*x
end function

elemental real function procedure_58_a(x)
    real, intent(in) :: x
    procedure_58_a = 4*x
end function

elemental double precision function procedure_58_b(x)
    double precision, intent(in) :: x
    procedure_58_b = 5*x
end function

elemental real function procedure_58_c(x)
    real, intent(in) :: x
    procedure_58_c = 6*x
end function

real function procedure_58_d(x, y)
    real, intent(in) :: x, y
    procedure_58_d = x*y
end function

program procedure_58
    use procedure_58_m, only: procedure_58_f, procedure_58_g, call_sin, &
        call_dsin_cos
    use procedure_58_own_m, only: call_own_sqrt
    implicit none
    real :: y(2)
    double precision :: d
    y = procedure_58_f([1.0, 2.0])
    print *, y
    if (any(abs(y - [2.0, 4.0]) > 1e-6)) error stop
    d = procedure_58_g(0.5d0)
    print *, d
    if (abs(d - 1.5d0) > 1d-12) error stop
    print *, call_sin(1.5)
    if (abs(call_sin(1.5) - 6.0) > 1e-6) error stop
    call call_dsin_cos(1.0d0, d)
    print *, d
    if (abs(d - 11.0d0) > 1d-6) error stop
    print *, call_own_sqrt(2.0, 3.0)
    if (abs(call_own_sqrt(2.0, 3.0) - 6.0) > 1e-6) error stop
end program
