module traits_runtime_factory_01_provider_m
    use traits_runtime_factory_01_contracts_m, only: IValue, finals, sums, requests
    implicit none
    private
    public :: construct
    type :: HiddenA
        integer, allocatable :: data(:)
    contains
        final :: finish_a
    end type
    type :: HiddenB
        integer :: factor, offset
        integer, allocatable :: data(:)
    contains
        final :: finish_b
    end type
    implements IValue :: HiddenA
        procedure, pass :: value => a_value
    end implements
    implements IValue :: HiddenB
        procedure, pass :: value => b_value
    end implements
contains
    integer function a_value(self)
        class(HiddenA), intent(in) :: self
        if (self%data(2) /= 18) error stop 1
        a_value = self%data(1)
    end function
    integer function b_value(self)
        class(HiddenB), intent(in) :: self
        if (self%data(2) /= 11) error stop 2
        b_value = self%factor * self%data(1) + self%offset
    end function
    subroutine finish_a(self)
        type(HiddenA), intent(inout) :: self
        if (.not. allocated(self%data)) error stop 3
        finals(0) = finals(0) + 1
        sums(0) = sums(0) + a_value(self)
        self%data = -777
    end subroutine
    subroutine finish_b(self)
        type(HiddenB), intent(inout) :: self
        if (.not. allocated(self%data)) error stop 4
        finals(1) = finals(1) + 1
        sums(1) = sums(1) + b_value(self)
        self%factor = -777
        self%data = -777
    end subroutine
    subroutine construct(choice, object)
        integer, intent(in) :: choice
        class(IValue), allocatable, intent(out) :: object
        if (allocated(object)) error stop 5
        requests = requests + 1
        if (choice == 0) then
            block
                type(HiddenA) :: seed
                allocate(seed%data(2))
                seed%data = [17, 18]
                allocate(object, source=seed)
            end block
        else if (choice == 1) then
            block
                type(HiddenB) :: seed
                seed%factor = 4
                seed%offset = 1
                allocate(seed%data(2))
                seed%data = [7, 11]
                allocate(object, source=seed)
            end block
        else
            error stop 6
        end if
    end subroutine
end module

function make_value(choice) result(object)
    use traits_runtime_factory_01_contracts_m, only: IValue
    use traits_runtime_factory_01_provider_m, only: construct
    implicit none
    integer, intent(in) :: choice
    class(IValue), allocatable :: object
    call construct(choice, object)
end function
