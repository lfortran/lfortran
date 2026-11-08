program gpu_metal_733
! A section assignment inside `do concurrent` evaluates its subscripts
! before storing any element (F2023 10.2.1.3). The offloaded element
! loop used to re-read a lower bound or stride that was an element of
! the target, so the first store changed the rest of the section.
implicit none
integer :: lo(4), b(4), a(6), p(2, 3), q(4)
integer :: n, i

lo = 1
do concurrent (i = 1:1)
    lo(lo(1):4) = 7
end do
if (any(lo /= 7)) error stop "lower bound element"

b = [0, 2, 0, 0]
do concurrent (i = 1:1)
    b(b(2):4) = 7
end do
if (b(1) /= 0 .or. any(b(2:4) /= 7)) error stop "lower bound not first element"

a = 1
do concurrent (i = 1:1)
    a(1:6:a(1)) = 7
end do
if (any(a /= 7)) error stop "stride element"

p = 1
do concurrent (i = 1:1)
    p(p(1, 1), 1:3) = 7
end do
if (any(p(1, :) /= 7) .or. any(p(2, :) /= 1)) error stop "scalar subscript element"

n = 3
q = 0
do concurrent (i = 1:1)
    q(2:n) = 5
end do
if (q(1) /= 0 .or. any(q(2:3) /= 5) .or. q(4) /= 0) error stop "scalar bound"
end program
