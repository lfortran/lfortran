module traits_runtime_pointer_separate_01_b
    use traits_runtime_pointer_separate_01_contracts
    implicit none
    private
    public :: install_b
    type :: Cell
        real(8) :: padding(3) = [1.0_8, 2.0_8, 3.0_8]
        integer :: n
    end type
    implements IValue :: Cell
        procedure, pass :: read_into => write_cell
        procedure, pass :: value => read_cell
    end implements
    type(Cell), target, save :: object
contains
    integer function read_cell(self)
        class(Cell), intent(in) :: self
        if (any(self%padding /= [1.0_8, 2.0_8, 3.0_8])) error stop 1
        read_cell = 2 * self%n + 1
    end function
    subroutine write_cell(self, result)
        class(Cell), intent(in) :: self
        integer, intent(out) :: result
        result = 2 * self%n + 1
    end subroutine
    subroutine install_b(view, n)
        class(IValue), pointer, intent(out) :: view
        integer, intent(in) :: n
        object%n = n
        view => object
    end subroutine
end module
