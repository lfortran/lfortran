subroutine h(a, n)
    type :: __lcompilers_frame_h
        real(4), dimension(:), pointer, reference :: a
    end type __lcompilers_frame_h
    type(__lcompilers_frame_h) :: __lcompilers_frame
    real(4), dimension(:), pointer :: __lcompilers_view_a
    integer(4) :: n
    real(4), dimension(n) :: a
    __lcompilers_view_a => a
    bind_reference(__lcompilers_frame%a, __lcompilers_view_a)
    call inner(__lcompilers_frame)
    contains
    subroutine inner(__lcompilers_frame)
        type(__lcompilers_frame_h), intent(in) :: __lcompilers_frame
        real(4), dimension(:), pointer, host_associated :: a => __lcompilers_frame%a
        a(1) = real(5, kind=4)
    end subroutine inner

end subroutine h

