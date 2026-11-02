program separate_compilation_68
use separate_compilation_68a
implicit none
integer :: k, x(3)
k = -5
x = 1
print *, size(seq(-5)), size(seq(k)), size(bounded(4, 1))
if (size(seq(-5)) /= 0) error stop 1
if (size(seq(k)) /= 0) error stop 2
if (any(seq(3) /= [1, 2, 3])) error stop 3
if (size(bounded(4, 1)) /= 0) error stop 4
if (any(bounded(4, 6) /= [4, 5, 6])) error stop 5
if (total(4, 1, x) /= 10) error stop 6
if (total(4, 6, x) /= 346) error stop 7
end program
