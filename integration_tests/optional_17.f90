! An `optional` statement applied to a dummy procedure declared by an
! interface body with arguments must keep the interface's arguments, and
! must not disturb a same-named procedure of the host module.
module optional_17_mod
implicit none
type :: t
    integer :: v
end type
integer :: counter = 0
contains

subroutine get_ptr(ptr, n)
integer, intent(out) :: ptr
integer, intent(in) :: n
ptr = 10*n
end subroutine

subroutine q()
counter = counter + 1
end subroutine

integer function times(a, n)
type(t), intent(in) :: a
integer, intent(in) :: n
times = a%v*n
end function

subroutine rd(x, get_ptr)
integer, intent(out) :: x
interface
    subroutine get_ptr(ptr, n)
    integer, intent(out) :: ptr
    integer, intent(in) :: n
    end subroutine
end interface
optional :: get_ptr
if (present(get_ptr)) then
    call get_ptr(x, 4)
else
    x = -1
end if
end subroutine

integer function apply(f)
optional :: f
interface
    integer function f(a, n)
    import :: t
    type(t), intent(in) :: a
    integer, intent(in) :: n
    end function
end interface
if (present(f)) then
    apply = f(t(5), 2)
else
    apply = -1
end if
end function

subroutine run(q)
optional :: q
interface
    subroutine q()
    end subroutine
end interface
if (present(q)) call q()
end subroutine

end module

program optional_17
use optional_17_mod
implicit none
integer :: x
call rd(x)
if (x /= -1) error stop
call rd(x, get_ptr)
if (x /= 40) error stop
if (apply() /= -1) error stop
if (apply(times) /= 10) error stop
call run()
if (counter /= 0) error stop
call run(q)
if (counter /= 1) error stop
call q()
if (counter /= 2) error stop
end program
