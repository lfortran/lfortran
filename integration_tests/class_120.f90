! Test that user-defined operators defined in a parent type are inherited
! by extended (child) types and can be used with child-type operands.
module class_120_mod
    implicit none
    type :: parent_t
        real :: x
    contains
        generic :: operator(.op.) => apply_op
        procedure, private :: apply_op
    end type
    type, extends(parent_t) :: child_t
    end type
contains
    function apply_op(self) result(res)
        class(parent_t), intent(in) :: self
        real :: res
        res = self%x
    end function
    function use_op(v) result(res)
        type(child_t), intent(in) :: v
        real :: res
        res = .op. v
    end function
end module class_120_mod

program class_120
    use class_120_mod, only: child_t, use_op
    implicit none
    type(child_t) :: c
    c%x = 3.0
    if (use_op(c) /= 3.0) error stop
    print *, "PASS"
end program
