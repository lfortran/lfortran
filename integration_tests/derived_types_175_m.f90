module derived_types_175_m
implicit none

type :: r1_t
    integer, pointer :: p(:) => null()
    integer :: z = 3
end type

integer, target :: mtgt(3) = [1, 2, 3]

! The same shape as derived_types_174, kept small so that it can be compiled
! a second time with --separate-compilation: there the module and the program
! are separate translation units, and only the one that has the program can
! set the elements' members up.
type(r1_t) :: marr(2)
type(r1_t) :: mctor(2) = r1_t(z=5)

contains

subroutine check_from_module()
    if (associated(marr(1)%p)) error stop 21
    if (associated(marr(2)%p)) error stop 22
    if (marr(1)%z /= 3) error stop 23
    if (mctor(2)%z /= 5) error stop 24
end subroutine

end module derived_types_175_m
