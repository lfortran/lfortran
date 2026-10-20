! The contracts of a dependency-injection container: a factory receives the
! container as a view of the trait it extends, and messages of the container
! and of a problem take a procedure pointer and a view of the container.
module traits_procarg_sep_01_oracle_contracts_m
    implicit none
    type, abstract :: IResolver
    contains
        procedure(resolve_interface), deferred :: resolve
    end type IResolver

    abstract interface
        function resolve_interface(self, key) result(res)
            import; implicit none
            class(IResolver), intent(in) :: self
            character(*), intent(in) :: key
            class(*), allocatable :: res
        end function resolve_interface
    end interface

    abstract interface
        function ifactory(c) result(res)
            import; implicit none
            class(IResolver), intent(in) :: c
            class(*), allocatable :: res
        end function ifactory
    end interface

    type, abstract, extends(IResolver) :: IContainer
    contains
        procedure(register_interface), deferred :: register
    end type IContainer

    abstract interface
        subroutine register_interface(self, key, factory)
            import; implicit none
            class(IContainer), intent(inout) :: self
            character(*), intent(in) :: key
            procedure(ifactory), pointer, intent(in) :: factory
        end subroutine register_interface
    end interface

    type, abstract :: IProblem
    contains
        procedure(registration_interface), deferred :: registration
    end type IProblem

    abstract interface
        subroutine registration_interface(self, c)
            import; implicit none
            class(IProblem), intent(in) :: self
            class(IContainer), intent(inout) :: c
        end subroutine registration_interface
    end interface

    type :: Mesh
        integer :: cells = 0
        real(8) :: width = 0
    end type Mesh
end module traits_procarg_sep_01_oracle_contracts_m
