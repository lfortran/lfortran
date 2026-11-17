module class_intent_out_reset_02_ext_m
    use class_intent_out_reset_02_base_m, only: base
    implicit none
    type, extends(base) :: child
        integer :: step = 2
        integer, allocatable :: work(:)
    contains
        procedure :: name => child_name
        procedure :: extra
    end type
contains
    integer function child_name(self)
        class(child), intent(in) :: self
        child_name = 2
    end function
    integer function extra(self)
        class(child), intent(in) :: self
        extra = 3
    end function
    integer function probe(x)
        class(base), intent(inout) :: x
        probe = 100 * x%name() + x%scale()
    end function
end module
