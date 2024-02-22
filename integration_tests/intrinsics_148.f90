program intrinsics_148
integer :: array(3, 4)
logical :: mask(3, 4)
integer :: vector(13)
integer :: res(13)
integer :: out

real :: array_3(3, 4, 5)
logical :: mask_3(3, 4, 5)
real :: vector_3(60)
real :: res_3(60)

array = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4])
mask = .false.
mask(1, 1) = .true.
vector = [114, 2414, 3142, 4626, 575, 633, 71, 89, 9123, 1011, 1112, 1213, 1314]

print *, pack(array, .true., vector)
print *, sum(pack(array, .true., vector))
out = sum(pack(array, .true., vector))
if (out /= 1392) error stop

print *, pack(array, mask, vector)
print *, sum(pack(array, mask, vector))
out = sum(pack(array, mask, vector))
if (out /= 25324) error stop

print *, pack(array, mask)
print *, sum(pack(array, mask))
out = sum(pack(array, mask))
if (out /= 1) error stop

array_3 = 12.141
mask_3 = .false.
mask_3(1, 1, 1) = .true.

vector_3 = -12.141
print *, pack(array_3, mask_3, vector_3)
print *, sum(pack(array_3, mask_3, vector_3))
if (abs(sum(pack(array_3, mask_3, vector_3)) + 704.177734) > 1e-6) error stop

end program
