module traits_runtime_inspection_provider_m
    use iso_c_binding, only: c_loc
    use traits_runtime_inspection_contracts_m
    implicit none
    private
    public :: acquire
    type, extends(Branch) :: HiddenLeaf
        integer :: padding(5) = 3
    contains
        final :: finish_hidden
    end type
    type(PublicCell), target, save :: first
    type(HiddenLeaf), target, save :: second
    implements IRich :: PublicCell
        procedure, pass :: value => public_value
        procedure, nopass :: label => public_label
    end implements
    implements IRich :: HiddenLeaf
        procedure, pass :: value => hidden_value
        procedure, nopass :: label => hidden_label
    end implements
contains
    integer function public_value(self)
        type(PublicCell), intent(in) :: self
        public_value = self%n
    end function
    integer function hidden_value(self)
        type(HiddenLeaf), intent(in) :: self
        hidden_value = self%n
    end function
    integer function public_label()
        public_label = 101
    end function
    integer function hidden_label()
        hidden_label = 202
    end function
    subroutine finish_hidden(self)
        type(HiddenLeaf), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
        self%n = -888
    end subroutine
    subroutine acquire(choice, n, view)
        integer, intent(in) :: choice, n
        class(IRich), pointer, intent(out) :: view
        if (choice == 1) then
            first%n = n
            view => first
            expected_address = c_loc(first%n)
        else
            second%n = n
            view => second
            expected_address = c_loc(second%n)
        end if
    end subroutine
end module

subroutine inspection_acquire(choice, n, view)
    use traits_runtime_inspection_contracts_m, only: IRich
    use traits_runtime_inspection_provider_m, only: acquire
    implicit none
    integer, intent(in) :: choice, n
    class(IRich), pointer, intent(out) :: view
    call acquire(choice, n, view)
end subroutine

function inspection_make(choice, n) result(owner)
    use traits_runtime_inspection_contracts_m, only: IRich
    use traits_runtime_inspection_provider_m, only: acquire
    implicit none
    integer, intent(in) :: choice, n
    class(IRich), allocatable :: owner
    class(IRich), pointer :: view
    call acquire(choice, n, view)
    allocate(owner, source=view)
    nullify(view)
end function
