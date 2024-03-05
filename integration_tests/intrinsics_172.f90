program intrinsics_172
use iso_fortran_env, only: int32, int64
implicit none
logical :: mask(3, 4)
logical :: mask_(6, 9)
logical :: mask_3(3, 4, 3)
logical :: mask_4(4, 5, 3, 1)

integer(kind=int32) :: res_4
integer(kind=int64) :: res_8

mask = reshape([ .true., .false., .true., .false., &
                 .true., .false., .true., .false., &
                 .true., .false., .true., .true.], [3, 4])

mask_ = .false.
mask_(1, 1) = .true.
mask_(1, 2) = .true.
mask_(5, 1) = .true.
mask_(5, 2) = .true.
mask_(6, 7) = .true.
mask_(4, 8) = .true.

mask_3 = .false.
mask_3(1, 1, :) = .true.
mask_3(1, 2, :) = .true.
mask_3(3, 1, :) = .true.
mask_3(2, 2, :) = .true.
mask_3(2, 4, :) = .true.
mask_3(3, 3, :) = .true.

mask_4 = .true.
mask_4(1, 1, 1, 1) = .false.
mask_4(1, 1, 1, :) = .false.
mask_4(1, 1, 1, :) = .false.
mask_4(1, 2, 1, :) = .false.
mask_4(1, 2, 3, :) = .false.


print *, count(mask)
if (count(mask) /= 7) error stop
print *, count(mask_)
if (count(mask_) /= 6) error stop
print *, count(mask_3)
if (count(mask_3) /= 18) error stop

print *, count(mask, 1)
if (sum(count(mask, 1)) /= 7) error stop
print *, count(mask, 2)
if (sum(count(mask, 2)) /= 7) error stop

print *, sum(count(mask_, 1))
if (sum(count(mask_, 1)) /= 6) error stop
print *, shape(count(mask_, 1))
print *, sum(count(mask_, 2))
if (sum(count(mask_, 2)) /= 6) error stop
print *, shape(count(mask_, 2))

print *, sum(count(mask_3, 1))
if (sum(count(mask_3, 1)) /= 18) error stop
print *, shape(count(mask_3, 1))
print *, sum(count(mask_3, 2))
if (sum(count(mask_3, 2)) /= 18) error stop
print *, shape(count(mask_3, 2))
print *, sum(count(mask_3, 3))
if (sum(count(mask_3, 3)) /= 18) error stop

print *, sum(count(mask_4, 1))
if (sum(count(mask_4, 1)) /= 57) error stop
print *, shape(count(mask_4, 1))

res_4 = count(mask_4, kind=4)
if (res_4 /= 57) error stop

res_8 = count(mask_4, kind=8)
if (res_8 /= 57) error stop

end program
