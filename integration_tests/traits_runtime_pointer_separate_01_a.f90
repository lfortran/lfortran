module traits_runtime_pointer_separate_01_a
    use traits_runtime_pointer_separate_01_contracts
    implicit none
    private
    public :: install_a
    type :: Cell
        integer :: n
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
        procedure, pass :: read_into => write_cell
    end implements
    type(Cell), target, save :: object
contains
    integer function read_cell(self)
        class(Cell), intent(in) :: self
        read_cell = self%n
    end function
    subroutine write_cell(self, result)
        class(Cell), intent(in) :: self
        integer, intent(out) :: result
        result = self%n
    end subroutine
    subroutine install_a(view, n)
        class(IValue), pointer, intent(out) :: view
        integer, intent(in) :: n
        object%n = n
        view => object
    end subroutine
end module
