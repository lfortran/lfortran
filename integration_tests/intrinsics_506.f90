program intrinsics_506
! sum/product/maxval/minval/iany with a DIM of kind 8, 2 and 1 (#16722)
implicit none
real :: v(3,2), s(2), t(3)
integer :: iv(3,2), r(2)
integer(8) :: d8
integer(2) :: d2
integer(1) :: d1
logical :: m(3,2)
v = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
iv = reshape([1, 2, 4, 8, 16, 32], [3, 2])
m = v > 1.5
d8 = 2
d2 = 2
d1 = 2

s = sum(v, dim=1_8)
if (any(s /= [6., 15.])) error stop
s = sum(v, 1_2)
if (any(s /= [6., 15.])) error stop
s = sum(v, 1_1, m)
if (any(s /= [5., 15.])) error stop
t = sum(v, dim=d8)
if (any(t /= [5., 7., 9.])) error stop
t = sum(v, d2, mask=m)
if (any(t /= [4., 7., 9.])) error stop

s = product(v, dim=1_8)
if (any(s /= [6., 120.])) error stop
t = product(v, dim=d2)
if (any(t /= [4., 10., 18.])) error stop
s = product(v, dim=1_2, mask=m)
if (any(s /= [6., 120.])) error stop

s = maxval(v, dim=1_2)
if (any(s /= [3., 6.])) error stop
t = maxval(v, dim=d8, mask=v < 5.5)
if (any(t /= [4., 5., 3.])) error stop
t = maxval(v, d1)
if (any(t /= [4., 5., 6.])) error stop

s = minval(v, dim=1_8)
if (any(s /= [1., 4.])) error stop
t = minval(v, dim=d2, mask=m)
if (any(t /= [4., 2., 3.])) error stop

r = iany(iv, dim=1_8)
if (any(r /= [7, 56])) error stop
print *, "ok"
end program
