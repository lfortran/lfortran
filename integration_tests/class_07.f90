module class_07_mod

private
    type, abstract :: tester
    contains
        procedure(testing_method), deferred, nopass :: testing
    end type tester

    abstract interface
        pure function testing_method() result(n)
            integer :: n
        end function testing_method
    end interface
end module

program class_07
use class_07_mod
end program
