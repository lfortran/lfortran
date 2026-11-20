! procedure(name), where name is itself declared by a procedure declaration
! statement: an external procedure whose abstract interface is defined later,
! a procedure pointer, or a dummy procedure.
module proc_ptr_50_m
    implicit none

    procedure(solver_iface) :: solver_dt

    abstract interface
        subroutine solver_iface(n, x)
            integer, intent(in) :: n
            real, intent(inout) :: x(n)
        end subroutine solver_iface
    end interface

    procedure(solver_dt), pointer :: solve => null()

    procedure(solver_iface), pointer :: solver_ptr => null()
    procedure(solver_ptr), pointer :: solve2 => null()

contains

    subroutine double_it(n, x)
        integer, intent(in) :: n
        real, intent(inout) :: x(n)
        x = 2.0*x
    end subroutine double_it

    subroutine apply(f, x)
        procedure(solver_iface) :: f
        real, intent(inout) :: x(:)
        procedure(f), pointer :: p
        procedure(solver_dt), pointer :: q
        p => f
        call p(size(x), x)
        q => f
        call q(size(x), x)
    end subroutine apply

end module proc_ptr_50_m

program proc_ptr_50
    use proc_ptr_50_m
    implicit none
    real :: x(3) = [1.0, 2.0, 3.0]
    procedure(solve), pointer :: local => null()

    if (associated(solve)) error stop
    solve => double_it
    call solve(3, x)
    if (any(x /= [2.0, 4.0, 6.0])) error stop

    solve2 => double_it
    call solve2(3, x)
    if (any(x /= [4.0, 8.0, 12.0])) error stop

    call apply(double_it, x)
    if (any(x /= [16.0, 32.0, 48.0])) error stop

    local => solve
    call local(3, x)
    if (any(x /= [32.0, 64.0, 96.0])) error stop
    print '(a, 3f6.1)', 'ok', x
end program proc_ptr_50
