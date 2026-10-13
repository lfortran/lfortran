! An intrinsic operator passed as an instantiation argument must be bound to a
! deferred function.
module template_intrinsic_op_01_m
    implicit none
    template sub_t{op}
        deferred interface
            subroutine op(x, y)
                real, intent(inout) :: x
                real, intent(in) :: y
            end subroutine
        end interface
    contains
        subroutine run(x, y)
            real, intent(inout) :: x
            real, intent(in) :: y
            call op(x, y)
        end subroutine
    end template
contains
    template subroutine apply_sub{op}(x, y)
        deferred interface
            subroutine op(x, y)
                real, intent(inout) :: x
                real, intent(in) :: y
            end subroutine
        end interface
        real, intent(inout) :: x
        real, intent(in) :: y
        call op(x, y)
    end subroutine
end module

program template_intrinsic_op_01
    use template_intrinsic_op_01_m
    implicit none
    instantiate sub_t{operator(+)}, only: run_add => run  ! {Error} the instantiation argument 'operator(+)' for 'op' requires a deferred function
    real :: x
    x = 1.0
    call apply_sub{operator(-)}(x, 2.0)  ! {Error} the instantiation argument 'operator(-)' for 'op' requires a deferred function
end program
