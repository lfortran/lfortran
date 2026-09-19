module derived_types_174_m
implicit none

type :: r1_t
    integer, pointer :: p(:) => null()
    integer :: z = 0
end type

type :: r2_t
    integer, pointer :: p(:,:) => null()
end type

type :: sp_t
    integer, pointer :: s => null()
    integer :: z = 0
end type

type :: al_t
    integer, allocatable :: a(:)
    integer :: z = 0
end type

type :: inner_t
    integer, pointer :: p(:) => null()
end type

type :: outer_t
    type(inner_t) :: in
    integer :: z = 0
end type

! A type whose members have non zero defaults next to the pointer component,
! so that the run time setup of the elements has to apply them as well.
type :: dflt_t
    integer, pointer :: p(:) => null()
    integer :: z = 7
    real :: r = 1.5
    character(3) :: c = "abc"
end type

! A type all of whose members are described by an all zero static
! initializer: its elements need no run time setup at all.
type :: pod_t
    integer :: z = 0
    real :: r = 0.0
end type

integer, target :: mtgt(3) = [1, 2, 3]

! Module level arrays of a derived type with a rank >= 1 pointer component.
! These are static data, so each element needs an array descriptor of its own
! to point at, which no static initializer can describe.
type(r1_t) :: marr(2)
type(r1_t) :: mctor(2) = r1_t(z=5)
type(r1_t) :: m2d(2,2)
type(r2_t) :: mr2(2)
type(outer_t) :: mnest(2)
type(dflt_t) :: mdflt(2)

! An array of a type that needs no run time setup: it must keep working.
type(pod_t) :: mpod(2)

! The same shape with components that were never affected: they must keep
! working unchanged.
type(al_t) :: mal(2)
type(sp_t) :: msp(2)

! Module level variables that are not a statically shaped array of a derived
! type, and were therefore already set up correctly.
type(r1_t) :: mscalar
type(r1_t) :: mscalar_c = r1_t(z=7)
type(r1_t), allocatable :: malloc(:)
type(r1_t), pointer :: mptr(:) => null()

contains

subroutine check_from_module()
    ! The module array read from inside the module, rather than from the
    ! program that uses it.
    if (associated(marr(1)%p)) error stop 61
    if (associated(marr(2)%p)) error stop 62
    if (mctor(1)%z /= 5) error stop 63
    if (associated(mctor(1)%p)) error stop 64
end subroutine

subroutine take(arg)
    type(r1_t), intent(in) :: arg
    if (associated(arg%p)) error stop 65
    if (arg%z /= 0) error stop 66
end subroutine

subroutine associate_through_dummy(arg)
    ! Written through, rather than only read: the component of the module
    ! array element is associated and assigned through the dummy argument.
    type(r1_t), intent(inout) :: arg
    if (associated(arg%p)) error stop 69
    allocate(arg%p(2))
    arg%p = [11, 12]
    arg%z = 13
end subroutine

subroutine saved_array()
    type(r1_t), save :: sarr(2)
    if (associated(sarr(1)%p)) error stop 67
    if (associated(sarr(2)%p)) error stop 68
end subroutine

end module derived_types_174_m
