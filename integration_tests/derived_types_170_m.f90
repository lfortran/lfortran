module derived_types_170_m
use iso_c_binding, only: c_ptr, c_null_ptr, c_associated
implicit none

type :: item_t
    integer :: h = 0
end type

type :: char_item_t
    character(3) :: s
end type

type(item_t), parameter :: z = item_t(4)
type(item_t), parameter :: pa(3) = [z, z, z]
type(item_t), parameter :: pb(3) = [item_t(5), item_t(6), item_t(7)]
type(item_t), parameter :: za(2) = [item_t(1), item_t(2)]
type(item_t), parameter :: grid(2,2) = reshape( &
    [item_t(1), item_t(2), item_t(3), item_t(4)], [2,2])
type(item_t), parameter :: shifted(0:1) = [item_t(9), item_t(10)]
type(item_t), parameter :: shifted_grid(0:1,-1:0) = reshape( &
    [item_t(11), item_t(12), item_t(13), item_t(14)], [2,2])
type(char_item_t), parameter :: char_params(2) = &
    [char_item_t("abc"), char_item_t("def")]
type(item_t) :: elem_from_parameter = za(2)
type(item_t), parameter :: elem_from_grid = grid(2,2)
integer, parameter :: grid_h = grid(2,2)%h
integer, parameter :: shifted_h = shifted(0)%h
integer, parameter :: shifted_grid_h = shifted_grid(1,0)%h
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
