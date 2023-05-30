module test

    private

    type, abstract :: tester
    contains
        procedure(testing_interface), deferred, pass(my_object), nopass  :: testing
    end type tester

    abstract interface
        subroutine testing_interface(my_object, a, b)
        class(tester), intent(inout) :: my_object
        integer, intent(inout) :: a, b
        end subroutine testing_interface
    end interface

end module test
