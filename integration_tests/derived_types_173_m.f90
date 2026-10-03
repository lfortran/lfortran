module derived_types_173_m
implicit none

type :: rank1_t
    integer, pointer :: p(:) => null()
    integer :: z = 0
end type

type :: rank2_t
    integer, pointer :: p(:,:) => null()
end type

type :: scalar_ptr_t
    integer, pointer :: s => null()
    integer :: z = 0
end type

type :: alloc_t
    integer, allocatable :: a(:)
    integer :: z = 0
end type

type :: b_t
    integer :: k
end type

! Derived type pointer array component.
type :: o_t
    type(b_t), pointer :: bp(:) => null()
end type

! The same, with a preceding component supplied by the constructor.
type :: ox_t
    integer :: x
    type(b_t), pointer :: bp(:) => null()
end type

! A parent and a child type, both with a pointer array component, so that a
! constructor has to build the nested parent constant as well.
type :: parent_t
    integer, pointer :: pp(:) => null()
    integer :: a = 0
end type

type, extends(parent_t) :: child_t
    integer, pointer :: cp(:) => null()
    integer :: b = 0
end type

! Module variables initialized by a structure constructor. Unlike the cases in
! the program body these are static data: the component cannot be set up by
! running code, it has to be a valid array descriptor in the object file.
type(rank1_t) :: mv = rank1_t(z=8)
type(rank1_t) :: mvn = rank1_t(null(), 6)
type(rank2_t) :: m2 = rank2_t()
type(o_t) :: mo = o_t()
type(ox_t) :: mox = ox_t(4, null())
type(child_t) :: mext = child_t(a=10, b=20)

contains

subroutine take(arg)
type(rank1_t), intent(in) :: arg
if (associated(arg%p)) error stop 29
if (arg%z /= 3) error stop 30
end subroutine

end module derived_types_173_m
