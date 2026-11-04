program intrinsics_505
! sum/product/maxval/minval/iany/iall/iparity with a default integer DIM
! when compiled with --descriptor-index-64 (#16722)
implicit none
real :: v(3,2), w(4), s(2), t(3)
integer :: iv(3,2), r(2)
integer :: d
logical :: m(3,2)
v = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
w = [1., 2., 3., 4.]
iv = reshape([1, 2, 4, 8, 16, 32], [3, 2])
m = v > 1.5
d = 2

s = sum(v, dim=1)
if (any(s /= [6., 15.])) error stop
s = sum(v, 1, mask=m)
if (any(s /= [5., 15.])) error stop
t = sum(v, dim=d)
if (any(t /= [5., 7., 9.])) error stop
t = sum(v, d, m)
if (any(t /= [4., 7., 9.])) error stop
if (sum(w, dim=1) /= 10.) error stop

s = product(v, dim=1)
if (any(s /= [6., 120.])) error stop
s = product(v, dim=1, mask=m)
if (any(s /= [6., 120.])) error stop
t = product(v, dim=d, mask=m)
if (any(t /= [4., 10., 18.])) error stop

s = maxval(v, dim=1)
if (any(s /= [3., 6.])) error stop
s = maxval(v, dim=1, mask=v < 5.5)
if (any(s /= [3., 5.])) error stop
t = maxval(v, d)
if (any(t /= [4., 5., 6.])) error stop

s = minval(v, dim=1)
if (any(s /= [1., 4.])) error stop
s = minval(v, dim=1, mask=m)
if (any(s /= [2., 4.])) error stop
t = minval(v, d)
if (any(t /= [1., 2., 3.])) error stop

r = iany(iv, dim=1)
if (any(r /= [7, 56])) error stop
r = iall(iv, dim=1)
if (any(r /= [0, 0])) error stop
r = iparity(iv, dim=1)
if (any(r /= [7, 56])) error stop
print *, "ok"
end program
