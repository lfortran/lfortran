module derived_types_121_child
use derived_types_121_module, only: ilp, state_type
implicit none

type, extends(state_type) :: child_type
end type child_type

contains

pure function get_message(flag) result(msg)
    integer(ilp), intent(in) :: flag
    character(len=:), allocatable :: msg
    if (flag == 0) then
        msg = 'ok'
    else
        msg = 'error'
    end if
end function get_message

pure function print_msg(this) result(msg)
    class(child_type), intent(in) :: this
    character(len=:), allocatable :: msg
    msg = get_message(this%state)
end function print_msg

end module derived_types_121_child

program derived_types_121
use derived_types_121_child
implicit none
type(child_type) :: c
character(len=:), allocatable :: msg

c%state = 0
msg = print_msg(c)
if (msg /= 'ok') error stop

c%state = 1
msg = print_msg(c)
if (msg /= 'error') error stop

print *, 'ok'
end program derived_types_121
