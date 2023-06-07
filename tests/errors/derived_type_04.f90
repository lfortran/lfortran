module test

    private

    type, abstract :: tester
    contains
        procedure(testing_interface), deferred, pass(my_object)  :: testing
    end type tester

    abstract interface
        subroutine testing_interface(a, b, c)
        integer, intent(inout) :: a, b, c
        end subroutine testing_interface
    end interface

end module test
