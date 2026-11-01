! The frame of a procedure host that captures a variable whose derived type
! is declared in the enclosing module, not in the procedure.
module host_assoc_frame_02_m
implicit none
type :: node
    integer :: v
end type
contains
subroutine h(x)
type(node), intent(inout) :: x
call inner()
contains
subroutine inner()
x%v = 1
end subroutine
end subroutine
end module
