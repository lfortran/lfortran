module traits_runtime_inspection_contracts_m
    use iso_c_binding, only: c_ptr
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: ILabel
        integer function label()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
    end interface
    abstract interface, extends(IChild + ILabel) :: IRich
    end interface
    type :: Root
        integer :: n = 0
    end type
    type, extends(Root) :: Branch
        integer :: extra = 7
    end type
    type, extends(Branch) :: PublicCell
        integer :: last = 11
    contains
        final :: finish_public
    end type
    type :: Other
        integer :: n
    end type
    integer :: finals = 0, total = 0
    type(c_ptr) :: expected_address
    interface
        subroutine inspection_acquire(choice, n, view)
            import :: IRich
            integer, intent(in) :: choice, n
            class(IRich), pointer, intent(out) :: view
        end subroutine
        function inspection_make(choice, n) result(owner)
            import :: IRich
            integer, intent(in) :: choice, n
            class(IRich), allocatable :: owner
        end function
        subroutine inspection_alternative(view, n)
            import :: IRich
            class(IRich), pointer, intent(in) :: view
            integer, intent(in) :: n
        end subroutine
    end interface
contains
    subroutine finish_public(self)
        type(PublicCell), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
        self%n = -777
    end subroutine
end module
