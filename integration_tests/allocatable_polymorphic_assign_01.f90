module allocatable_polymorphic_assign_01_mod

    type, abstract :: base_t
    contains
        procedure(assign_op), deferred :: assign
        generic :: assignment(=) => assign
    end type base_t

    abstract interface
        subroutine assign_op(self, other)
            import
            class(base_t), intent(out) :: self
            class(base_t), intent(in)  :: other
        end subroutine assign_op
    end interface

contains

    subroutine test_assign_from_function()
        class(base_t), allocatable :: a
        a = make_base()
    end subroutine test_assign_from_function

    function make_base() result(b)
        class(base_t), allocatable :: b
    end function make_base

end module allocatable_polymorphic_assign_01_mod

program allocatable_polymorphic_assign_01
    implicit none
    print *, "PASS"
end program allocatable_polymorphic_assign_01
