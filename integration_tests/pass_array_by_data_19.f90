module pass_array_by_data_19_mod
implicit none
contains

subroutine run(f, x)
    interface
        subroutine f(y)
            real, intent(inout) :: y(:)
        end subroutine
    end interface
    real, intent(inout), target :: x(:)
    real, pointer :: p(:)
    call f(x(1:2))
    call f(x(1:5:2))
    p => x(2:6:2)
    call f(p)
end subroutine

real function apply(g, x) result(r)
    interface
        real function g(y)
            real, intent(in) :: y(:)
        end function
    end interface
    real, intent(in), target :: x(:)
    real, pointer :: p(:)
    r = g(x(1:2))
    r = r + 10*g(x(2:6:2))
    p => x(1:5:2)
    r = r + 100*g(p)
end function

subroutine twice(y)
    real, intent(inout) :: y(:)
    y = 2*y
end subroutine

real function total(y)
    real, intent(in) :: y(:)
    total = sum(y)
end function

end module

program pass_array_by_data_19
use pass_array_by_data_19_mod, only: run, apply, twice, total
implicit none
real :: x(6), r

! Module procedures taking a dummy procedure
x = 1
call run(twice, x)
print *, x
if (any(x /= [4, 4, 2, 2, 2, 2])) error stop
x = [1, 2, 3, 4, 5, 6]
r = apply(total, x)
print *, r
if (abs(r - (3 + 10*12 + 100*9)) > 1e-5) error stop

! Internal procedures taking a dummy procedure
x = 1
call run_internal(twice_internal, x)
print *, x
if (any(x /= [4, 4, 2, 2, 2, 2])) error stop
x = [1, 2, 3, 4, 5, 6]
r = apply_internal(total_internal, x)
print *, r
if (abs(r - (3 + 10*12 + 100*9)) > 1e-5) error stop

contains

subroutine run_internal(f, x)
    interface
        subroutine f(y)
            real, intent(inout) :: y(:)
        end subroutine
    end interface
    real, intent(inout), target :: x(:)
    real, pointer :: p(:)
    call f(x(1:2))
    call f(x(1:5:2))
    p => x(2:6:2)
    call f(p)
end subroutine

real function apply_internal(g, x) result(r)
    interface
        real function g(y)
            real, intent(in) :: y(:)
        end function
    end interface
    real, intent(in), target :: x(:)
    real, pointer :: p(:)
    r = g(x(1:2))
    r = r + 10*g(x(2:6:2))
    p => x(1:5:2)
    r = r + 100*g(p)
end function

subroutine twice_internal(y)
    real, intent(inout) :: y(:)
    y = 2*y
end subroutine

real function total_internal(y)
    real, intent(in) :: y(:)
    total_internal = sum(y)
end function

end program
