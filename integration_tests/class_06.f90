module class_06_mod

private

type, abstract :: tester
contains
    procedure(testing), deferred, pass :: testing
end type tester

abstract interface
    subroutine testing(self)
    import tester
    class(tester), intent(inout) :: self
    end subroutine testing
end interface

end module

program class_06
use class_06_mod
end program
