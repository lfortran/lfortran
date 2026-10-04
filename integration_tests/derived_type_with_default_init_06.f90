module derived_type_with_default_init_06_mod
implicit none

abstract interface
    integer function iface()
    end function
end interface

type :: tf
    character(len=5) :: s = "aaaaa"
    integer :: n = 0
    procedure(iface), pointer, nopass :: fp => null()
contains
    procedure :: get_n
    final :: finalize_tf
end type

type :: u
    type(tf) :: fin_part = tf("hello", 9, null())
end type

contains

integer function one()
    one = 1
end function

integer function get_n(self)
    class(tf), intent(in) :: self
    get_n = self%n
end function

subroutine finalize_tf(self)
    type(tf), intent(inout) :: self
    self%n = -2
end subroutine

subroutine reset(x)
    type(u), intent(out) :: x
end subroutine

subroutine check(x)
    type(u), intent(in) :: x
    if (x%fin_part%s /= "hello" .or. x%fin_part%get_n() /= 9) error stop 1
    if (associated(x%fin_part%fp)) error stop 2
end subroutine

end module

program derived_type_with_default_init_06
use derived_type_with_default_init_06_mod
implicit none
integer :: i
type(u) :: g

do i = 1, 3
    call dirty_stack()
    call local_default()
    g%fin_part%s = "bye"
    g%fin_part%n = -1
    g%fin_part%fp => one
    call reset(g)
    call check(g)
end do
print *, "ok"

contains

subroutine dirty_stack()
    integer :: junk(256)
    junk = -1
    if (junk(1) /= -1) error stop 3
end subroutine

subroutine local_default()
    type(u) :: v
    call check(v)
end subroutine

end program
