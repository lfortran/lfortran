module traits_runtime_result_03_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Payload
        integer :: n
    contains
        final :: finish
    end type
    implements IValue :: Payload
        procedure, pass :: value => payload_value
    end implements
    type(Payload) :: seed
    integer :: finals = 0, total = 0
contains
    integer function payload_value(self)
        class(Payload), intent(in) :: self
        payload_value = self%n
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
        self%n = -777
    end subroutine
    function make(values, delta) result(object)
        integer, intent(in) :: values(:)
        integer, optional, intent(in) :: delta
        class(IValue), allocatable :: object
        seed%n = sum(values)
        if (present(delta)) seed%n = seed%n + delta
        allocate(object, source=seed)
    end function
    integer function read_value(object)
        class(IValue), intent(in) :: object
        read_value = object%value()
    end function
end module

module traits_runtime_result_03_facade
    use traits_runtime_result_03_m, only: factory_interface => make, IValue, read_value, finals, total
end module

program traits_runtime_result_03
    use traits_runtime_result_03_facade
    implicit none
    type :: FactoryHolder
        procedure(factory_interface), pointer, nopass :: make => null()
    end type
    type(FactoryHolder) :: holder
    procedure(factory_interface), pointer :: factory
    integer :: n

    factory => factory_interface
    n = read_value(factory([8, 9]))
    if (n /= 17 .or. finals /= 1 .or. total /= 17) error stop 1
    call apply(factory_interface)
    if (finals /= 2 .or. total /= 40) error stop 2
    holder%make => factory_interface
    n = read_value(holder%make([30], 1))
    if (n /= 31 .or. finals /= 3 .or. total /= 71) error stop 3
    block
        procedure(factory_interface), pointer :: local_factory
        local_factory => factory_interface
        n = read_value(local_factory([40], 1))
    end block
    if (n /= 41 .or. finals /= 4 .or. total /= 112) error stop 4
contains
    subroutine apply(fn)
        procedure(factory_interface) :: fn
        integer :: result
        result = read_value(fn([10, 13], 0))
        if (result /= 23) error stop 5
    end subroutine
end program
