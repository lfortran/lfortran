program allocate_68
! Test allocate with source= for scalar allocatables
implicit none

integer, allocatable :: src_i, obj_i
real, allocatable :: src_r, obj_r
logical, allocatable :: src_l, obj_l

! Integer scalar allocatable with source=
allocate(src_i, source=99)
if (src_i /= 99) error stop
allocate(obj_i, source=src_i)
if (obj_i /= 99) error stop
deallocate(src_i)
deallocate(obj_i)

! Real scalar allocatable with source=
allocate(src_r, source=3.14)
allocate(obj_r, source=src_r)
if (abs(obj_r - 3.14) > 1.0e-5) error stop
deallocate(src_r)
deallocate(obj_r)

! Logical scalar allocatable with source=
allocate(src_l, source=.true.)
allocate(obj_l, source=src_l)
if (obj_l .neqv. .true.) error stop
deallocate(src_l)
deallocate(obj_l)

print *, "PASS"
end program allocate_68
