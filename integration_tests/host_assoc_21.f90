program host_assoc_21
use iso_c_binding
implicit none
integer :: n
abstract interface
    subroutine callback(values)
        import n
        integer, intent(out) :: values(n)
    end subroutine
end interface
procedure(callback), pointer :: p
type(c_funptr) :: address
n = 2
p => fill
call invoke(worker)
address = export(plain)
if (.not. c_associated(address)) error stop 2
call consume(plain)
contains
subroutine invoke(f)
    interface
        subroutine f()
        end subroutine
    end interface
    call f()
end subroutine
subroutine worker()
    integer :: values(n)
    call p(values)
    if (any(values /= n)) error stop 1
end subroutine
subroutine fill(values)
    integer, intent(out) :: values(n)
    values = n
end subroutine
function export(f) result(address)
    procedure(plain) :: f
    type(c_funptr) :: address
    address = c_funloc(f)
end function
subroutine plain() bind(c)
end subroutine
subroutine consume(f) bind(c)
    procedure(plain) :: f
    call f()
end subroutine
end program
