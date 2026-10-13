! An intrinsic unary operator passed in an inline instantiation inside a
! templated procedure (#14731).
module template_intrinsic_op_03_m
    implicit none
contains
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

    template function outer{T}(x) result(r)
        deferred type :: T
        real, intent(in) :: x
        real :: r
        r = apply{operator(-)}(x)
    end function
end module

program template_intrinsic_op_03
    use template_intrinsic_op_03_m
    implicit none
    if (abs(outer{integer}(2.0) + 2.0) > 1.0e-6) error stop 1
    print *, "OK"
end program
