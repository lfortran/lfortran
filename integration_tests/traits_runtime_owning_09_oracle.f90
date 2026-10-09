! GFortran 16 omits inherited bindings in enclosing intrinsic assignment.
! This direct-component control checks the selected concrete override instead.
module traits_runtime_owning_09_oracle_m
    implicit none
    integer :: assignments = 0
    type, abstract :: Base
    contains
        procedure(assign_signature), deferred :: assign
        generic :: assignment(=) => assign
    end type
    type, extends(Base) :: Child
        integer :: n = 10
    contains
        procedure :: assign => assign_child
    end type
    abstract interface
        subroutine assign_signature(self, other)
            import Base, Child
            class(Base), intent(inout) :: self
            type(Child), intent(in) :: other
        end subroutine
    end interface
    type :: Envelope
        type(Child) :: part
    end type
contains
    subroutine assign_child(self, other)
        class(Child), intent(inout) :: self
        type(Child), intent(in) :: other
        self%n = self%n + other%n
        assignments = assignments + 1
    end subroutine
end module

program traits_runtime_owning_09_oracle
    use traits_runtime_owning_09_oracle_m
    implicit none
    type(Envelope) :: source, destination
    source%part%n = 3
    destination%part = source%part
    if (assignments /= 1 .or. destination%part%n /= 13) error stop 1
end program
