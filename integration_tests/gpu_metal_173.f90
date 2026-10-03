program gpu_metal_173
! Test: do concurrent with non-1 lower bound arrays + liveout scalars
! Verifies that the Metal backend correctly handles:
!   1. Arrays with lower bound 0 (n(0:2)) — index offset must use actual
!      lower bound, not assume 1-based.
!   2. Scalars assigned inside do concurrent that are read after the loop
!      (liveout scalars) — must be passed as device buffers and read back.
implicit none
integer :: n(0:2), r, l
n = [10, 20, 30]
r = 0
do concurrent(l = 1:1)
    r = n(1)
end do
if (r /= 20) error stop
print *, "PASS"
end program
