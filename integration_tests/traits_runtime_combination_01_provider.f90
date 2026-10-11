module traits_runtime_combination_01_provider_m
    use iso_c_binding, only: c_int, c_loc
    use traits_runtime_combination_01_contracts_m, only: IRich, PublicBox, &
        expected_address, observed_address, finals, final_sum
    implicit none
    private
    public :: acquire, update, release
    type :: HiddenBox
        integer :: padding(5) = 42
        integer(c_int) :: payload = 0
        integer, allocatable :: data(:)
    contains
        final :: finalize_hidden
    end type
    type(PublicBox), target, save :: first
    type(HiddenBox), allocatable, target, save :: second
    implements IRich :: PublicBox
        procedure, pass :: value => public_value
        procedure, pass(self) :: scaled => public_scaled
        procedure, nopass :: label => public_label
    end implements
    implements IRich :: HiddenBox
        procedure, pass :: value => hidden_value
        procedure, pass(self) :: scaled => hidden_scaled
        procedure, nopass :: label => hidden_label
    end implements
contains
    integer function public_value(self)
        type(PublicBox), target, intent(in) :: self
        observed_address = c_loc(self%payload)
        public_value = self%payload
    end function
    integer function public_scaled(factor, self)
        integer, intent(in) :: factor
        type(PublicBox), target, intent(in) :: self
        observed_address = c_loc(self%payload)
        public_scaled = factor * self%payload
    end function
    integer function public_label()
        public_label = 101
    end function
    integer function hidden_value(self)
        type(HiddenBox), target, intent(in) :: self
        observed_address = c_loc(self%payload)
        if (allocated(self%data)) then
            if (self%data(1) /= self%payload .or. self%data(2) /= self%payload + 1) error stop 1301
        end if
        hidden_value = self%payload
    end function
    integer function hidden_scaled(factor, self)
        integer, intent(in) :: factor
        type(HiddenBox), target, intent(in) :: self
        observed_address = c_loc(self%payload)
        hidden_scaled = factor * self%payload
    end function
    integer function hidden_label()
        hidden_label = 202
    end function
    subroutine finalize_hidden(self)
        type(HiddenBox), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%payload
        if (allocated(self%data)) self%data = -999
        self%payload = -888
    end subroutine
    subroutine update(choice, n)
        integer, intent(in) :: choice, n
        if (choice == 1) then
            first%payload = n
        else
            if (.not. allocated(second)) allocate(second)
            second%payload = n
            if (.not. allocated(second%data)) allocate(second%data(2))
            second%data = [n, n + 1]
        end if
    end subroutine
    subroutine release()
        if (allocated(second)) deallocate(second)
    end subroutine
    subroutine acquire(choice, n, object)
        integer, intent(in) :: choice, n
        class(IRich), pointer, intent(out) :: object
        call update(choice, n)
        if (choice == 1) then
            expected_address = c_loc(first%payload)
            object => first
        else
            expected_address = c_loc(second%payload)
            object => second
        end if
    end subroutine
end module

subroutine combination_acquire(choice, n, object)
    use traits_runtime_combination_01_contracts_m, only: IRich
    use traits_runtime_combination_01_provider_m, only: acquire
    implicit none
    integer, intent(in) :: choice, n
    class(IRich), pointer, intent(out) :: object
    call acquire(choice, n, object)
end subroutine

subroutine combination_acquire_anonymous(choice, n, object)
    use traits_runtime_combination_01_contracts_m, only: IValue, ILabel, IScale, IRich
    use traits_runtime_combination_01_provider_m, only: acquire
    implicit none
    integer, intent(in) :: choice, n
    class(IScale + IValue + ILabel), pointer, intent(out) :: object
    class(IRich), pointer :: selected
    call acquire(choice, n, selected)
    object => selected
    nullify(selected)
end subroutine

subroutine combination_update(choice, n)
    use traits_runtime_combination_01_provider_m, only: update
    implicit none
    integer, intent(in) :: choice, n
    call update(choice, n)
end subroutine

subroutine combination_release()
    use traits_runtime_combination_01_provider_m, only: release
    implicit none
    call release()
end subroutine
