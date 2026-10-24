program array_size_empty_triplet_01
! A triplet whose stride is longer than the distance from its start
! to its end has no elements, and neither has a section with two
! empty triplets.
implicit none
integer :: a(10), m(5, 5), n
a = 0
m = 0
n = 0
if (size(a(1:0:2)) /= 0) error stop
if (size(a(1:0:2), 1) /= 0) error stop
if (size(a(1:n:2)) /= 0) error stop
if (size(a(1:n:2), 1) /= 0) error stop
if (size(a(1:5:-1), 1) /= 0) error stop
if (size(a(1:n+5:-1), 1) /= 0) error stop
if (size(a(2:1:-3)) /= 1) error stop
if (size(m(5:1, 5:1)) /= 0) error stop
if (size(m(5:n+1, 5:n+1)) /= 0) error stop
if (size(m(1:5:2, 4:3)) /= 0) error stop
print *, size(a(1:n:2)), size(m(5:n+1, 5:n+1)), size(a(2:1:-3))
end program
