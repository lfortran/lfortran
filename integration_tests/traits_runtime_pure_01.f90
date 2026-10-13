! Runtime trait syntax is an LFortran extension.
module traits_runtime_pure_01_m
    implicit none
    abstract interface :: IValue
        pure function value(n) result(r)
            integer, intent(in) :: n
            integer :: r
        end function
        pure subroutine increment(n)
            integer, intent(inout) :: n
        end subroutine
    end interface
    type :: Payload
        integer :: n
    end type
    implements IValue :: Payload
        procedure, pass :: value => read_value
        procedure, nopass :: increment => add_one
    end implements
contains
    pure function read_value(self, n) result(r)
        type(Payload), intent(in) :: self
        integer, intent(in) :: n
        integer :: r
        r = self%n + n
    end function
    pure subroutine add_one(n)
        integer, intent(inout) :: n
        n = n + 1
    end subroutine
    pure function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value(4)
        call object%increment(r)
    end function
    pure subroutine forward(object, r)
        class(IValue), intent(in) :: object
        integer, intent(out) :: r
        r = observe(object)
        call object%increment(r)
    end subroutine
end module

program traits_runtime_pure_01
    use traits_runtime_pure_01_m
    implicit none
    type(Payload) :: object
    integer :: r
    object%n = 19
    if (observe(object) /= 24) error stop 1
    call forward(object, r)
    if (r /= 25) error stop 2
end program
