program derived_types_174
use derived_types_174_m
implicit none
type(r1_t) :: larr(2)

! A module level array of a derived type with a rank 1 pointer component and
! no initializer: reading the component must see the unassociated state.
if (associated(marr(1)%p)) error stop 1
if (associated(marr(2)%p)) error stop 2
if (marr(1)%z /= 0) error stop 3

! The elements are independent of each other: associating one must leave the
! other one alone.
marr(1)%p => mtgt
if (.not. associated(marr(1)%p)) error stop 4
if (associated(marr(2)%p)) error stop 5
if (size(marr(1)%p) /= 3) error stop 6
if (any(marr(1)%p /= [1, 2, 3])) error stop 7
marr(1)%p => null()
if (associated(marr(1)%p)) error stop 8

! The same array with a structure constructor initializer, which is applied
! before the program starts.
if (mctor(1)%z /= 5) error stop 9
if (mctor(2)%z /= 5) error stop 10
if (associated(mctor(1)%p)) error stop 11
if (associated(mctor(2)%p)) error stop 12

! Again, the two elements must not share one descriptor.
mctor(2)%p => mtgt
if (associated(mctor(1)%p)) error stop 13
if (.not. associated(mctor(2)%p)) error stop 14
if (size(mctor(2)%p) /= 3) error stop 15
mctor(2)%p => null()
if (associated(mctor(2)%p)) error stop 16

! A rank 2 array of the derived type.
if (associated(m2d(1,1)%p)) error stop 17
if (associated(m2d(2,2)%p)) error stop 18
m2d(2,2)%p => mtgt
if (associated(m2d(1,1)%p)) error stop 19
if (associated(m2d(1,2)%p)) error stop 20
if (associated(m2d(2,1)%p)) error stop 21
if (.not. associated(m2d(2,2)%p)) error stop 22
m2d(2,2)%p => null()

! A rank 2 pointer component.
if (associated(mr2(1)%p)) error stop 23
if (associated(mr2(2)%p)) error stop 24

! A pointer component one level deeper, inside a derived type component.
if (associated(mnest(1)%in%p)) error stop 25
if (associated(mnest(2)%in%p)) error stop 26
mnest(1)%in%p => mtgt
if (.not. associated(mnest(1)%in%p)) error stop 27
if (associated(mnest(2)%in%p)) error stop 28
mnest(1)%in%p => null()

! Allocating the component of one element, rather than pointing it at a
! target, and then reading it back.
allocate(marr(2)%p(3))
marr(2)%p = [7, 8, 9]
if (.not. associated(marr(2)%p)) error stop 29
if (size(marr(2)%p) /= 3) error stop 30
if (any(marr(2)%p /= [7, 8, 9])) error stop 31
if (associated(marr(1)%p)) error stop 32
deallocate(marr(2)%p)
if (associated(marr(2)%p)) error stop 33

! An allocatable array component of a module array element.
if (allocated(mal(1)%a)) error stop 34
if (allocated(mal(2)%a)) error stop 35
allocate(mal(1)%a(2))
mal(1)%a = [4, 5]
if (allocated(mal(2)%a)) error stop 36
if (any(mal(1)%a /= [4, 5])) error stop 37
deallocate(mal(1)%a)

! A scalar pointer component of a module array element.
if (associated(msp(1)%s)) error stop 38
if (associated(msp(2)%s)) error stop 39
if (msp(1)%z /= 0) error stop 40

! A module level scalar of the same derived type, with and without a
! structure constructor initializer.
if (associated(mscalar%p)) error stop 41
if (mscalar%z /= 0) error stop 42
if (associated(mscalar_c%p)) error stop 43
if (mscalar_c%z /= 7) error stop 44

! A module level allocatable array of the derived type.
allocate(malloc(2))
if (associated(malloc(1)%p)) error stop 45
if (associated(malloc(2)%p)) error stop 46
malloc(1)%p => mtgt
if (associated(malloc(2)%p)) error stop 47
malloc(1)%p => null()
deallocate(malloc)

! A module level pointer array of the derived type.
allocate(mptr(2))
if (associated(mptr(1)%p)) error stop 48
if (associated(mptr(2)%p)) error stop 49
deallocate(mptr)

! A local array of the derived type, in the program.
if (associated(larr(1)%p)) error stop 50
if (associated(larr(2)%p)) error stop 51
larr(1)%p => mtgt
if (associated(larr(2)%p)) error stop 52
larr(1)%p => null()

! The non zero defaults of the components of such an array: the run time
! setup of an element has to apply them, the same way as for the module
! level scalar of the same type.
if (mdflt(1)%z /= 7) error stop 55
if (mdflt(2)%z /= 7) error stop 56
if (abs(mdflt(1)%r - 1.5) > 1e-6) error stop 57
if (mdflt(2)%c /= "abc") error stop 58
if (associated(mdflt(1)%p)) error stop 59
mdflt(1)%p => mtgt
if (associated(mdflt(2)%p)) error stop 60
mdflt(1)%p => null()

! An array of a type that needs no run time setup of its elements.
if (mpod(1)%z /= 0) error stop 70
if (mpod(2)%z /= 0) error stop 71
if (abs(mpod(1)%r) > 1e-6) error stop 72
mpod(1)%z = 3
if (mpod(2)%z /= 0) error stop 73

! An element of the module array passed to a procedure.
call take(marr(1))

! An element of the module array written through a dummy argument.
call associate_through_dummy(marr(2))
if (.not. associated(marr(2)%p)) error stop 74
if (size(marr(2)%p) /= 2) error stop 75
if (any(marr(2)%p /= [11, 12])) error stop 76
if (marr(2)%z /= 13) error stop 77
if (associated(marr(1)%p)) error stop 78
deallocate(marr(2)%p)

! The module array read from inside its own module.
call check_from_module()

! A save array of the derived type, inside a procedure.
call saved_array()
call saved_array()

! A save array of the derived type, in the program.
call program_saved_array()

print *, "ok"

contains

subroutine program_saved_array()
    type(r1_t), save :: psarr(2)
    if (associated(psarr(1)%p)) error stop 53
    if (associated(psarr(2)%p)) error stop 54
end subroutine

end program derived_types_174
