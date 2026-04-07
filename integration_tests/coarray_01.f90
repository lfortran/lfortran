program coarray_01
implicit none
integer :: x[*]
integer :: y(3)[*]

x = this_image()
if (x /= 1) error stop

! Coarray reference x[1] should resolve to x in single-image mode
if (x[1] /= 1) error stop

! Test with array coarray
y = [10, 20, 30]
if (y(1)[1] /= 10) error stop
if (y(2)[1] /= 20) error stop
if (y(3)[1] /= 30) error stop

sync all

if (num_images() /= 1) error stop

print *, "ok"
end program coarray_01
