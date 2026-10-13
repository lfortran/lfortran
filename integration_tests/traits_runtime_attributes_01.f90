! Runtime trait syntax is an LFortran extension.
module traits_runtime_attributes_01_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Payload
        integer :: n
    end type
    implements IValue :: Payload
        procedure, pass :: value => read_value
    end implements
contains
    function read_value(self) result(r)
        type(Payload), intent(in) :: self
        integer :: r
        r = self%n
    end function
end module

function observe_attributes(object) result(r)
    use traits_runtime_attributes_01_m, only: IValue
    implicit none
    class(IValue) :: object
    intent(in) :: object
    integer :: r
    r = object%value()
end function

subroutine check_attributes(object)
    use traits_runtime_attributes_01_m, only: IValue
    implicit none
    class(IValue) :: object
    intent(in) :: object
    if (object%value() /= 19) error stop 2
end subroutine

program traits_runtime_attributes_01
    use traits_runtime_attributes_01_m
    implicit none
    interface
        function observe_attributes(object) result(r)
            import :: IValue
            class(IValue) :: object
            intent(in) :: object
            integer :: r
        end function
        subroutine check_attributes(object)
            import :: IValue
            class(IValue) :: object
            intent(in) :: object
        end subroutine
    end interface
    type(Payload) :: object
    object%n = 19
    if (observe_attributes(object=object) /= 19) error stop 1
    call check_attributes(object=object)
end program
