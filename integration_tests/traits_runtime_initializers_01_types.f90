module traits_runtime_initializers_01_types
    use traits_runtime_initializers_01_contracts, only: IAdmissible
    implicit none
    private
    public :: Admitted, last_output, last_input

    integer :: last_output = 0, last_input = 0

    type, sealed :: Admitted
        integer :: n = 0
    end type Admitted

    implements IAdmissible :: Admitted
        initial :: admitted_from_integer
        procedure, pass :: output => admitted_output
        procedure, pass :: input => admitted_input
    end implements Admitted

contains

    function admitted_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Admitted)      :: res
        res%n = 10 * n
    end function admitted_from_integer

    subroutine admitted_output(self)
        type(Admitted), intent(in) :: self
        last_output = self%n
        print '(a,i0)', 'I am ', self%n
    end subroutine admitted_output

    subroutine admitted_input(self, filename)
        type(Admitted), intent(in) :: self
        character(*), intent(in)   :: filename
        last_input = self%n + len(filename)
    end subroutine admitted_input
end module traits_runtime_initializers_01_types
