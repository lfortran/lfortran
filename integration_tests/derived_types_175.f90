program derived_types_175
use derived_types_175_m
implicit none

! A module level array of a derived type with a rank 1 pointer component,
! read from a translation unit that only declares the module variable.
if (associated(marr(1)%p)) error stop 1
if (associated(marr(2)%p)) error stop 2
if (marr(1)%z /= 3) error stop 3

marr(1)%p => mtgt
if (.not. associated(marr(1)%p)) error stop 4
if (associated(marr(2)%p)) error stop 5
if (any(marr(1)%p /= [1, 2, 3])) error stop 6
marr(1)%p => null()

! The same array with a structure constructor initializer.
if (mctor(1)%z /= 5) error stop 7
if (associated(mctor(1)%p)) error stop 8
if (associated(mctor(2)%p)) error stop 9
mctor(2)%p => mtgt
if (associated(mctor(1)%p)) error stop 10
mctor(2)%p => null()

! Allocating the component of one element through the module variable.
allocate(marr(2)%p(2))
marr(2)%p = [7, 8]
if (associated(marr(1)%p)) error stop 11
if (any(marr(2)%p /= [7, 8])) error stop 12
deallocate(marr(2)%p)

call check_from_module()

print *, "ok"

end program derived_types_175
