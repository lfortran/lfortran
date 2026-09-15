module derived_type_with_default_init_03_mod
implicit none

abstract interface
    integer function iface()
    end function
end interface

type :: tc
    integer :: i = 1
end type

type :: ta
    integer, pointer :: p => null()
    procedure(iface), pointer, nopass :: fp => null()
    type(tc), pointer :: tp => null()
    class(tc), pointer :: cp => null()
    character(len=:), pointer :: sp => null()
    real, pointer :: pa(:) => null()
    integer, allocatable :: ai
    character(:), allocatable :: s
    integer, allocatable :: a(:)
    class(tc), allocatable :: c
    character(len=5) :: k = "aaaaa"
end type

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
    type(ta) :: part = ta(null(), null(), null(), null(), null(), null(), &
        null(), null(), null(), null(), "hello")
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
    if (associated(x%part%p)) error stop 1
    if (associated(x%part%fp)) error stop 2
    if (associated(x%part%tp)) error stop 3
    if (associated(x%part%cp)) error stop 4
    if (associated(x%part%sp)) error stop 5
    if (associated(x%part%pa)) error stop 6
    if (allocated(x%part%ai) .or. allocated(x%part%s)) error stop 7
    if (allocated(x%part%a) .or. allocated(x%part%c)) error stop 8
    if (x%part%k /= "hello") error stop 9
    if (x%fin_part%s /= "hello" .or. x%fin_part%get_n() /= 9) error stop 11
    if (associated(x%fin_part%fp)) error stop 12
end subroutine

end module

program derived_type_with_default_init_03
use derived_type_with_default_init_03_mod
implicit none
integer :: i
integer, target :: tgt
type(tc), target :: ttgt
type(u) :: g

do i = 1, 3
    call dirty_stack()
    call local_default()
    allocate(g%part%a(4), g%part%ai)
    g%part%s = "dyn"
    allocate(tc :: g%part%c)
    g%part%p => tgt
    g%part%tp => ttgt
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
    if (junk(1) /= -1) error stop 10
end subroutine

subroutine local_default()
    type(u) :: v
    call check(v)
end subroutine

end program
