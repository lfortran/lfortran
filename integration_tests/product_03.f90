program product_03
! MASK of PRODUCT (and the other reduction intrinsics) may be a scalar:
! it is conformable with any array (F2023 16.9.165, 3.32).
implicit none
integer :: a(6) = [1, 2, 3, 4, 5, 6]
integer :: m2(2, 3)
logical :: t_s, f_s
m2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
t_s = .true.
f_s = .false.

! scalar mask used alongside array masks in the same scope: each call
! must get a matching runtime function, not reuse the array-mask one
if (product(a, mask=f_s) /= 1) error stop 1
if (product(a, mask=t_s) /= 720) error stop 2
if (product(a, mask=a > 3) /= 120) error stop 3

if (sum(a, mask=f_s) /= 0) error stop 4
if (sum(a, mask=t_s) /= 21) error stop 5
if (sum(a, mask=a > 3) /= 15) error stop 6

if (maxval(a, mask=f_s) /= -huge(1) - 1) error stop 7
if (minval(a, mask=f_s) /= huge(1)) error stop 8
if (minval(a, mask=t_s) /= 1) error stop 9

! positional scalar mask
if (product(a, f_s) /= 1) error stop 10
if (sum(a, t_s) /= 21) error stop 11

! dim together with a scalar mask
if (any(product(m2, dim=1, mask=f_s) /= 1)) error stop 12
if (any(sum(m2, dim=1, mask=t_s) /= [3, 7, 11])) error stop 13

if (iparity(a, mask=f_s) /= 0) error stop 14

print *, "ok"
end program product_03
