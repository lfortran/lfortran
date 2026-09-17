program derived_types_170
use derived_types_170_m
implicit none
type(holder_t) :: holder
type(item_t) :: loc(3)
type(item_t), parameter :: local_array(2) = [item_t(1), item_t(2)]
type(item_t), parameter :: local_copy(2) = local_array
type(c_ptr) :: q

loc = pa
if (any(loc%h /= 4)) error stop 1
if (elem_from_parameter%h /= 2) error stop 2
if (any(module_array%h /= 4)) error stop 3
if (any(holder%parts%h /= 4)) error stop 4
if (local_copy(2)%h /= 2 .or. local_array(2)%h /= 2) error stop 5
if (pb(2)%h /= 6) error stop 6
if (any(pb%h /= [5, 6, 7])) error stop 8
q = cptrs(1)
call check_cptr(q)
end program derived_types_170
