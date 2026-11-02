program procedure_63
implicit none
abstract interface
    real function callback(y)
        real, intent(in) :: y(:)
    end function
    subroutine sub_callback(y)
        real, intent(inout) :: y(:)
    end subroutine
end interface
real :: a(1), b(2), result

a(1) = 1.0
b(1) = 3.0
b(2) = 4.0
call apply_order(a, args)
if (a(1) /= 2.0) error stop
call apply_late(a, args, b)
if (a(1) /= 3.0 .or. b(2) /= 4.0) error stop
call apply_early(args, a, b)
if (a(1) /= 4.0 .or. b(2) /= 4.0) error stop
call apply_sub(a, args1, b)
if (a(1) /= 14.0 .or. b(1) /= 13.0) error stop
call apply_local(args, result)
if (result /= 6.0) error stop
call forward(args, a, b)
if (a(1) /= 16.0 .or. b(2) /= 14.0) error stop
call forward_named_sub(a, b)
if (a(1) /= 26.0 .or. b(1) /= 23.0) error stop

contains

subroutine apply_order(x, f)
    real, intent(inout) :: x(1)
    procedure(callback) :: f
    x(1) = f(x)
end subroutine

subroutine apply_late(x, f, z)
    real, intent(inout) :: x(1), z(2)
    procedure(callback) :: f
    x(1) = f(x)
    z(2) = f(z)
end subroutine

subroutine apply_early(f, x, z)
    procedure(callback) :: f
    real, intent(inout) :: x(1), z(2)
    x(1) = f(x)
    z(2) = f(z)
end subroutine

subroutine apply_sub(x, f, z)
    real, intent(inout) :: x(1), z(2)
    procedure(sub_callback) :: f
    call f(x)
    call f(z)
end subroutine

subroutine apply_local(f, value)
    procedure(callback) :: f
    real, intent(out) :: value
    real :: x(3)
    x(1) = 5.0
    value = f(x)
end subroutine

subroutine forward(f, x, z)
    procedure(callback) :: f
    real, intent(inout) :: x(1), z(2)
    call apply_late(x, f, z)
    call apply_late(x, args, z)
end subroutine

subroutine forward_named_sub(x, z)
    real, intent(inout) :: x(1), z(2)
    integer :: args
    args = 7
    call apply_sub(x, args1, z)
    if (args /= 7) error stop
end subroutine

real function args(y)
    real, intent(in) :: y(:)
    args = y(1) + 1.0
end function

subroutine args1(y)
    real, intent(inout) :: y(:)
    y(1) = y(1) + 10.0
end subroutine
end program
