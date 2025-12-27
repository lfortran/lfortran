module intentout_optional_deallocate_01_mod
    implicit none

    type :: msg_t
        character(len=:), allocatable :: msg
    end type msg_t

contains

    subroutine set_msg(x, s)
        type(msg_t), intent(out), optional :: x
        character(len=*), intent(in) :: s
        if (present(x)) then
            x%msg = s
        end if
    end subroutine set_msg

end module intentout_optional_deallocate_01_mod

program intentout_optional_deallocate_01
    use intentout_optional_deallocate_01_mod, only: msg_t, set_msg
    implicit none

    type(msg_t) :: v

    call set_msg(v, "hello")
    if (.not. allocated(v%msg)) error stop
    if (v%msg /= "hello") error stop

    call set_msg(s="skip")

    if (.not. allocated(v%msg)) error stop
    if (v%msg /= "hello") error stop
end program intentout_optional_deallocate_01

