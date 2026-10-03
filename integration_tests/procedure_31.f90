! Test that a procedure pointer with a derived-type allocatable array in its
! interface can be passed as an actual argument to a non-pointer dummy procedure.
! Regression test for two issues:
! 1) semantic type check was not stripping Pointer(FunctionType),
! 2) LLVM call lowering did an extra load for procedure-pointer actual args.
program procedure_31
    implicit none

    type :: t
        integer :: val
    end type t

    interface
        subroutine iface(x)
            import :: t
            type(t), allocatable, intent(out) :: x(:)
        end subroutine iface
    end interface

    procedure(iface), pointer :: f
    type(t), allocatable :: result(:)

    f => mysub
    call sub(f, result)
    if (result(1)%val /= 42) error stop

contains

    subroutine mysub(x)
        type(t), allocatable, intent(out) :: x(:)
        allocate(x(1))
        x(1)%val = 42
    end subroutine mysub

    subroutine sub(p, result)
        procedure(iface) :: p
        type(t), allocatable, intent(out) :: result(:)
        call p(result)
    end subroutine sub

end program procedure_31
