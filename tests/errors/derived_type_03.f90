module test

    private

    type, abstract :: tester
    contains
        procedure(testing_interface), deferred  :: testing
    end type tester

    abstract interface
        subroutine testing_interface(self)
        integer, intent(inout) :: self
        end subroutine testing_interface
    end interface

end module test
