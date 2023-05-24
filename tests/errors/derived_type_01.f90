module test

    private

    type, abstract :: tester
    contains
        procedure(testing_interface), deferred  :: testing
    end type tester

end module test
