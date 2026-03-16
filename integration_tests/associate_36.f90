! Test: procedure pointer call inside associate block with array args.
! Regression test for ICE in pass_array_by_data when an AssociateBlock
! symbol is visited before the procedure-pointer variable it calls.
module associate_36_mod
    implicit none

    abstract interface
        function my_func_i(x) result(f)
            double precision, intent(in) :: x(:)
            double precision, allocatable :: f(:)
        end function
    end interface

contains

    function compute(initializer) result(res)
        procedure(my_func_i), pointer :: initializer
        double precision, allocatable :: res(:)
        double precision :: input(3)

        input = [1.0d0, 2.0d0, 3.0d0]

        associate(values => initializer(input))
            res = values
        end associate
    end function

end module

program associate_36
    use associate_36_mod
    implicit none
    procedure(my_func_i), pointer :: f
    double precision, allocatable :: result(:)

    f => double_it
    result = compute(f)
    print *, result
    if (size(result) /= 3) error stop
    if (abs(result(1) - 2.0d0) > 1.0d-12) error stop
    if (abs(result(2) - 4.0d0) > 1.0d-12) error stop
    if (abs(result(3) - 6.0d0) > 1.0d-12) error stop

contains

    function double_it(x) result(f)
        double precision, intent(in) :: x(:)
        double precision, allocatable :: f(:)
        f = x * 2.0d0
    end function

end program
