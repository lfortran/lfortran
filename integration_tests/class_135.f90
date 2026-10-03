program class_135
! Test class(*) allocatable array assignment
implicit none

class(*), allocatable :: src(:), dst(:)

! Allocate and assign from integer array
allocate(src, source=[5, 7, 11])

! Copy to pre-allocated destination
allocate(dst, mold=src)
dst = src

! Verify values via select type
select type(dst)
type is (integer)
    if (dst(1) /= 5) error stop
    if (dst(2) /= 7) error stop
    if (dst(3) /= 11) error stop
class default
    error stop
end select

deallocate(src)
deallocate(dst)

! Test with real values
allocate(src, source=[1.5, 2.5, 3.5])
allocate(dst, mold=src)
dst = src

select type(dst)
type is (real)
    if (abs(dst(1) - 1.5) > 1.0e-6) error stop
    if (abs(dst(2) - 2.5) > 1.0e-6) error stop
    if (abs(dst(3) - 3.5) > 1.0e-6) error stop
class default
    error stop
end select

print *, "PASS"
end program
