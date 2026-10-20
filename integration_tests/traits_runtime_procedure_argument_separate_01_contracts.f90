! The contracts of a dependency-injection container: a factory receives the
! container as a view of the trait it extends, and messages of the container
! and of a problem take a procedure pointer and a view of the container.
module traits_procarg_sep_01_contracts_m
    implicit none
    abstract interface :: IResolver
        function resolve(self, key) result(res)
            type(itself), pass, intent(in) :: self
            character(*), intent(in) :: key
            class(*), allocatable :: res
        end function resolve
    end interface

    abstract interface
        function ifactory(c) result(res)
            import; implicit none
            class(IResolver), intent(in) :: c
            class(*), allocatable :: res
        end function ifactory
    end interface

    abstract interface, extends(IResolver) :: IContainer
        subroutine register(self, key, factory)
            type(itself), pass, intent(inout) :: self
            character(*), intent(in) :: key
            procedure(ifactory), pointer, intent(in) :: factory
        end subroutine register
    end interface

    abstract interface :: IProblem
        subroutine registration(self, c)
            type(itself), pass, intent(in) :: self
            class(IContainer), intent(inout) :: c
        end subroutine registration
    end interface

    type :: Mesh
        integer :: cells = 0
        real(8) :: width = 0
    end type Mesh
end module traits_procarg_sep_01_contracts_m
