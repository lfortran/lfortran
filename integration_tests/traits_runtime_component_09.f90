module traits_runtime_component_09_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IReader
        function read{IValue :: T}(x) result(n)
            type(T), intent(in) :: x
            integer :: n
        end function
    end interface
    type :: Payload
        integer :: n = 7
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Reader
        integer :: scale = 3
    end type
    implements IReader :: Reader
        procedure :: read => read_value
    end implements
    type :: Holder
        class(IReader), allocatable :: reader
    end type
contains
    integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    function read_value{IValue :: T}(self, x) result(n)
        type(Reader), intent(in) :: self
        type(T), intent(in) :: x
        integer :: n
        n = self%scale * x%value()
    end function
end module

program traits_runtime_component_09
    use traits_runtime_component_09_m
    implicit none
    type(Reader) :: source
    type(Payload) :: payload_value_object
    type(Holder) :: a, b
    a%reader = source
    if (a%reader%read(payload_value_object) /= 21) error stop 1
    if (a%reader%read{Payload}(payload_value_object) /= 21) error stop 2
    b = a
    deallocate(a%reader)
    if (b%reader%read{Payload}(payload_value_object) /= 21) error stop 3
    deallocate(b%reader)
end program
