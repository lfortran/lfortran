module host_assoc_18_module
implicit none
abstract interface
    subroutine callback(value)
        integer, intent(out) :: value
    end subroutine
end interface
type :: holder
    procedure(callback), pointer, nopass :: p => null()
end type
procedure(callback), pointer :: global_target => null()
contains
subroutine set_out(p, target)
    procedure(callback), pointer, intent(out) :: p
    procedure(callback), pointer, intent(in) :: target
    p => target
end subroutine
subroutine set_inout(p, target)
    procedure(callback), pointer, intent(inout) :: p
    procedure(callback), pointer, intent(in) :: target
    p => target
end subroutine
function copy_function(p) result(q)
    procedure(callback), pointer, intent(in) :: p
    procedure(callback), pointer :: q
    q => p
end function
subroutine invoke(p, value)
    procedure(callback) :: p
    integer, intent(out) :: value
    call p(value)
end subroutine
subroutine hold_target(p, value)
    procedure(callback) :: p
    integer, intent(out) :: value
    global_target => plain
    call p(value)
end subroutine
subroutine maybe(value, p)
    integer, intent(out) :: value
    procedure(callback), pointer, intent(in), optional :: p
    value = -2
    if (present(p)) then
        value = -3
        if (associated(p)) call p(value)
    end if
end subroutine
subroutine plain(value)
    integer, intent(out) :: value
    value = -1
end subroutine
end module
