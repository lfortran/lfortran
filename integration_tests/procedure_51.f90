module procedure_51_mod
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    abstract interface
        real(dp) function scale_iface(x, factor)
            import :: dp
            real(dp), intent(in) :: x
            real(dp), intent(in), optional :: factor
        end function
        subroutine add_iface(x, y)
            integer, intent(inout) :: x
            integer, intent(in) :: y
        end subroutine
    end interface
    procedure(scale_iface) :: procedure_51_scale
contains
    real(dp) function apply(f, x)
        procedure(scale_iface) :: f
        real(dp), intent(in) :: x
        apply = f(x, 3.0_dp)
    end function
end module

real(kind(1.0d0)) function procedure_51_scale(x, factor)
    real(kind(1.0d0)), intent(in) :: x
    real(kind(1.0d0)), intent(in), optional :: factor
    if (present(factor)) then
        procedure_51_scale = factor*x
    else
        procedure_51_scale = 10*x
    end if
end function

elemental real function procedure_51_twice(x)
    real, intent(in) :: x
    procedure_51_twice = 2*x
end function

subroutine procedure_51_add(x, y)
    integer, intent(inout) :: x
    integer, intent(in) :: y
    x = x + y
end subroutine

program procedure_51
    use procedure_51_mod, only: dp, add_iface, apply, procedure_51_scale
    implicit none
    abstract interface
        elemental real function twice_iface(x)
            real, intent(in) :: x
        end function
    end interface
    procedure(twice_iface) :: procedure_51_twice
    procedure(add_iface) :: procedure_51_add
    real :: a(3)
    integer :: i
    if (abs(procedure_51_twice(0.5) - 1.0) > 1e-6) error stop
    a = procedure_51_twice([1.0, 2.0, 3.0])
    print *, a
    if (any(abs(a - [2.0, 4.0, 6.0]) > 1e-6)) error stop
    if (abs(procedure_51_scale(2.0_dp) - 20.0_dp) > 1e-12_dp) error stop
    if (abs(procedure_51_scale(factor=0.5_dp, x=4.0_dp) - 2.0_dp) > 1e-12_dp) error stop
    if (abs(apply(procedure_51_scale, 2.0_dp) - 6.0_dp) > 1e-12_dp) error stop
    i = 1
    call procedure_51_add(i, 41)
    print *, i
    if (i /= 42) error stop
end program
