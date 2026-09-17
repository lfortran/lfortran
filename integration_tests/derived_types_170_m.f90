module derived_types_170_m
use iso_c_binding, only: c_ptr, c_null_ptr, c_associated
implicit none

type :: item_t
    integer :: h = 0
end type

type(item_t), parameter :: z = item_t(4)
type(item_t), parameter :: pa(3) = [z, z, z]
type(item_t), parameter :: pb(3) = [item_t(5), item_t(6), item_t(7)]
type(item_t), parameter :: za(2) = [item_t(1), item_t(2)]
type(item_t) :: elem_from_parameter = za(2)
type(item_t) :: module_array(3) = [item_t(4), item_t(4), item_t(4)]
type(c_ptr), parameter :: cptrs(2) = [c_null_ptr, c_null_ptr]

type :: holder_t
    type(item_t) :: parts(2) = [item_t(4), item_t(4)]
end type

contains

subroutine check_cptr(q)
type(c_ptr), intent(in) :: q
if (c_associated(q)) error stop 7
end subroutine

end module derived_types_170_m

