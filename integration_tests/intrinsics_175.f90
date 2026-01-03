program intrinsics_75
integer :: array(3, 4)
logical :: mask(3, 4)
integer :: s(1)

array = reshape((/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12/), shape(array))
mask = .true.
mask(1, :) = .false.
mask(3, 2) = .false.

print *, pack(array, mask)
s = shape(pack(array, mask))
print *, s
if (s(1) /= 7) error stop
contains
end program
