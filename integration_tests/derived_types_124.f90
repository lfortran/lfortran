module derived_types_124_mod
    implicit none

    type, abstract :: BaseType
    contains
        procedure(assign_iface), deferred :: assign_impl
        generic, public :: assignment(=) => assign_impl
    end type BaseType

    abstract interface
        subroutine assign_iface(self, other)
            import
            class(BaseType), intent(out) :: self
            class(BaseType), intent(in)  :: other
        end subroutine assign_iface
    end interface

    type, extends(BaseType) :: ConcreteType
        integer :: val = 0
    contains
        procedure :: assign_impl => concrete_assign
    end type ConcreteType

contains

    subroutine concrete_assign(self, other)
        class(ConcreteType), intent(out) :: self
        class(BaseType), intent(in) :: other
        select type(other)
        type is (ConcreteType)
            self%val = other%val + 10
        end select
    end subroutine concrete_assign

    subroutine test_polymorphic_assignment()
        class(BaseType), allocatable :: a, b
        allocate(ConcreteType :: a)
        allocate(ConcreteType :: b)
        select type(b)
        type is (ConcreteType)
            b%val = 5
        end select
        a = b
        select type(a)
        type is (ConcreteType)
            print *, a%val
            if (a%val /= 15) error stop
        class default
            error stop
        end select
    end subroutine test_polymorphic_assignment

end module derived_types_124_mod

program derived_types_124
    use derived_types_124_mod
    implicit none
    call test_polymorphic_assignment()
    print *, "PASS"
end program derived_types_124
