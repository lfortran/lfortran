program derived_types_170
use derived_types_170_m
implicit none
type(holder_t) :: holder
type(item_t) :: loc(3)
type(item_t), parameter :: local_array(2) = [item_t(1), item_t(2)]
type(item_t), parameter :: local_copy(2) = local_array
type(char_item_t) :: char_copy(2)
type(item_t), parameter :: local_grid(2,2) = reshape( &
    [item_t(21), item_t(22), item_t(23), item_t(24)], [2,2])
type(item_t), parameter :: local_grid_elem = local_grid(2,2)
integer, parameter :: local_grid_h = local_grid(2,2)%h
type(item_t), parameter :: local_shifted(-2:-1) = [item_t(31), item_t(32)]
integer, parameter :: local_shifted_h = local_shifted(-1)%h
type(c_ptr) :: q

loc = pa
if (any(loc%h /= 4)) error stop 1
if (elem_from_parameter%h /= 2) error stop 2
if (any(module_array%h /= 4)) error stop 3
if (any(holder%parts%h /= 4)) error stop 4
if (local_copy(2)%h /= 2 .or. local_array(2)%h /= 2) error stop 5
if (pb(2)%h /= 6) error stop 6
if (any(pb%h /= [5, 6, 7])) error stop 8
if (elem_from_grid%h /= 4 .or. grid_h /= 4) error stop 9
if (shifted_h /= 9 .or. shifted_grid_h /= 14) error stop 10
if (local_grid_elem%h /= 24 .or. local_grid_h /= 24) error stop 11
if (local_shifted_h /= 32) error stop 12
if (grid_order(1,1)%h /= 41 .or. grid_order(2,1)%h /= 43) error stop 17
if (grid_order(1,2)%h /= 42 .or. grid_order(2,2)%h /= 44) error stop 18
if (grid_order_h /= 43) error stop 19
if (grid_pad(1,1)%h /= 45 .or. grid_pad(2,1)%h /= 46) error stop 20
if (grid_pad(1,2)%h /= 47 .or. grid_pad(2,2)%h /= 48) error stop 21
if (grid_pad_h /= 47) error stop 22
if (cube(1,1,1)%h /= 51 .or. cube(2,1,1)%h /= 52) error stop 23
if (cube(1,2,1)%h /= 53 .or. cube(2,2,2)%h /= 58) error stop 24
if (cube_h /= 58) error stop 25
char_copy = char_params
if (char_params(1)%s /= "abc" .or. char_params(2)%s /= "def") error stop 13
if (char_copy(1)%s /= "abc" .or. char_copy(2)%s /= "def") error stop 14
char_copy(1)%s = "xyz"
if (char_copy(1)%s /= "xyz" .or. char_copy(2)%s /= "def") error stop 15
if (char_params(1)%s /= "abc" .or. char_params(2)%s /= "def") error stop 16
if (char_grid(1,1)%s /= "abc" .or. char_grid(2,1)%s /= "ghi") error stop 26
if (char_grid(1,2)%s /= "def" .or. char_grid(2,2)%s /= "jkl") error stop 27
q = cptrs(1)
call check_cptr(q)
call check_cptr_grid()
end program derived_types_170
