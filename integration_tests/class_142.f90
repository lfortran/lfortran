! Test: defined assignment on polymorphic allocatable components
! Verifies that user-defined assignment(=) is called (not intrinsic
! assignment) when assigning class(...) allocatable members.

module class_142_mod
    implicit none

    type, abstract :: base_type
    contains
        procedure, non_overridable, private :: copy
        generic :: assignment(=) => copy
        procedure(copy_iface), deferred :: copy_impl
    end type

    abstract interface
        subroutine copy_iface(lhs, rhs)
            import base_type
            class(base_type), intent(inout) :: lhs
            class(base_type), intent(in) :: rhs
        end subroutine
    end interface

    type, extends(base_type) :: container
        integer, pointer :: data => null()
    contains
        procedure :: copy_impl => container_copy
    end type

    type :: node
        class(base_type), allocatable :: value
    end type

contains

    subroutine copy(lhs, rhs)
        class(base_type), intent(inout) :: lhs
        class(base_type), intent(in) :: rhs
        call lhs%copy_impl(rhs)
    end subroutine

    subroutine container_copy(lhs, rhs)
        class(container), intent(inout) :: lhs
        class(base_type), intent(in) :: rhs
        select type (rhs)
        type is (container)
            if (associated(lhs%data)) deallocate(lhs%data)
            if (associated(rhs%data)) allocate(lhs%data, source=rhs%data)
        end select
    end subroutine

end module

program class_142
    use class_142_mod
    implicit none

    type(node) :: n1, n2

    allocate(container :: n1%value)
    select type (v => n1%value)
    type is (container)
        allocate(v%data, source=42)
    end select

    ! Copy via polymorphic defined assignment on allocatable component
    allocate(n2%value, mold=n1%value)
    n2%value = n1%value

    ! Modify original — copy should be unaffected if deep copy happened
    select type (v => n1%value)
    type is (container)
        v%data = 99
    end select

    ! Verify deep copy: n2's data should still be 42
    select type (v => n2%value)
    type is (container)
        if (.not. associated(v%data)) error stop
        if (v%data /= 42) error stop
    end select

    print *, "PASS"
end program
