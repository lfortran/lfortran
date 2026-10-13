module traits_runtime_generic_04_m
    use traits_runtime_generic_01_contracts_m, only: IValue
    implicit none
    abstract interface :: IPair
        function combine{IValue :: Left, IValue :: Right}(first, second) result(r)
            type(Left), intent(in) :: first
            type(Right), intent(in) :: second
            integer(8) :: r
        end function
    end interface
    type :: PairAlgorithm
        integer :: bias
    end type
    implements IPair :: PairAlgorithm
        procedure, pass(self) :: combine
    end implements
contains
    function combine{IValue :: X, IValue :: Y}(a, self, b) result(r)
        type(X), intent(in) :: a
        class(PairAlgorithm), intent(in) :: self
        type(Y), intent(in) :: b
        integer(8) :: r
        r = 1000_8*a%value() + b%value() + self%bias
    end function
end module

program traits_runtime_generic_04
    use traits_runtime_generic_04_m
    use traits_runtime_generic_01_matrix_client_m, only: LateValue, PaddedValue, AlternateValue, argument_finals
    implicit none
    type(LateValue) :: left
    type(PaddedValue) :: padded
    type(AlternateValue) :: right
    type(PairAlgorithm) :: provider
    class(IPair), allocatable :: object
    left%payload = 37
    padded%payload = 37
    padded%prefix = [1000.d0, -9.d0, 42.d0]
    right%payload = 37
    provider%bias = 5
    allocate(object, source=provider)
    if (object%combine(left, right) /= 37079_8) error stop 1
    if (object%combine{AlternateValue, LateValue}(right, left) /= 74042_8) error stop 2
    if (object%combine{PaddedValue, AlternateValue}(padded, right) /= 37079_8) error stop 3
    if (provider%combine(right, padded) /= 74042_8) error stop 4
    deallocate(object)
    if (argument_finals /= 0) error stop 5
    if (left%payload /= 37 .or. right%payload /= 37 .or. padded%payload /= 37) error stop 6
    if (any(padded%prefix /= [1000.d0, -9.d0, 42.d0])) error stop 7
end program
