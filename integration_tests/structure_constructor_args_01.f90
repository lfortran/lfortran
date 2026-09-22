! null() as a structure constructor argument has the type of its component
module structure_constructor_args_01_m
implicit none
abstract interface
    integer function f_i()
    end function
end interface
type :: base_t
    integer :: k = 0
end type
type :: a_t
    integer, allocatable :: al(:)
    integer, allocatable :: as
    integer, pointer :: p => null()
    procedure(f_i), pointer, nopass :: fp => null()
    type(a_t), pointer :: next => null()
    class(base_t), pointer :: cp => null()
    integer :: x = 1
end type
type :: ptr_t
    integer, pointer :: p => null()
    procedure(f_i), pointer, nopass :: fp => null()
    type(a_t), pointer :: next => null()
    class(base_t), pointer :: cp => null()
    integer :: x = 1
end type
type :: u_t
    type(ptr_t) :: part = ptr_t(null(), null(), null(), null(), 6)
    ! null() for the allocatable components of a component default
    type(a_t) :: apart = a_t(null(), null(), null(), null(), null(), null(), 13)
end type
type(a_t) :: mv = a_t(null(), null(), null(), null(), null(), null(), 7)
type(a_t), parameter :: pa = a_t(null(), null(), null(), null(), null(), null(), 10)
contains
subroutine check(a, x)
    type(a_t), intent(in) :: a
    integer, intent(in) :: x
    if (allocated(a%al)) error stop 1
    if (allocated(a%as)) error stop 2
    if (associated(a%p)) error stop 3
    if (associated(a%fp)) error stop 4
    if (associated(a%next)) error stop 5
    if (associated(a%cp)) error stop 6
    if (a%x /= x) error stop 7
end subroutine
subroutine from_parameter()
    type(a_t) :: la
    la = pa
    call check(la, 10)
end subroutine
subroutine mixed(n)
    integer, intent(in) :: n
    type(a_t) :: la
    type(u_t) :: lu
    la = a_t(null(), null(), null(), null(), null(), null(), n)
    call check(la, n)
    la = a_t(x=n + 1, next=null(), al=null(), fp=null())
    call check(la, n + 1)
    if (associated(lu%part%p)) error stop 8
    if (associated(lu%part%fp)) error stop 9
    if (associated(lu%part%next)) error stop 10
    if (associated(lu%part%cp)) error stop 11
    if (lu%part%x /= 6) error stop 12
    call check(lu%apart, 13)
end subroutine
end module

program structure_constructor_args_01
use structure_constructor_args_01_m
implicit none
type(a_t) :: lv = a_t(null(), null(), null(), null(), null(), null(), 8)
type(a_t) :: la
call check(mv, 7)
call check(lv, 8)
la = a_t(null(), null(), null(), null(), null(), null(), 9)
call check(la, 9)
call from_parameter()
call mixed(20)
print *, "ok"
end program
