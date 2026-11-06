module host_assoc_frame_02_m
implicit none
type :: node
    integer(4) :: v
end type node

contains

subroutine h(x)
    type :: __lcompilers_frame_h
        type(node), reference :: x
    end type __lcompilers_frame_h
    type(__lcompilers_frame_h) :: __lcompilers_frame
    type(node), intent(inout) :: x
    bind_reference(__lcompilers_frame%x, x)
    call inner(__lcompilers_frame)
    contains
    subroutine inner(__lcompilers_frame)
        type(__lcompilers_frame_h), intent(in) :: __lcompilers_frame
        type(node), host_associated :: x => __lcompilers_frame%x
        x%v = 1
    end subroutine inner

end subroutine h

end module host_assoc_frame_02_m

