! An intrinsic operator passed as an instantiation argument must be bound to a
! deferred function, and an intrinsic unary operator must apply to the type of
! its argument (#14731).
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

    template subroutine apply_sub1{op}(x)
        deferred interface
            subroutine op(x)
                real, intent(inout) :: x
            end subroutine
        end interface
        real, intent(inout) :: x
        call op(x)
    end subroutine

    template function apply{op}(x) result(r)
        deferred interface
            pure real function op(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(in) :: x
        real :: r
        r = op(x)
    end function
end module

program template_intrinsic_op_01
    use template_intrinsic_op_01_m
    implicit none
    instantiate sub_t{operator(+)}, only: run_add => run  ! {Error} the instantiation argument 'operator(+)' for 'op' requires a deferred function
    real :: x
    x = 1.0
    call apply_sub{operator(-)}(x, 2.0)  ! {Error} the instantiation argument 'operator(-)' for 'op' requires a deferred function
    call apply_sub1{operator(-)}(x)  ! {Error} the instantiation argument 'operator(-)' for 'op' requires a deferred function
    print *, apply{operator(.not.)}(2.0)  ! {Error} the intrinsic operator `.not.` cannot be applied to the argument of 'op', of type real(4)
end program
