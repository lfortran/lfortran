! `z = matmul(x, w) + b` with reallocation of the target on assignment:
! a target of another shape, or unallocated, gets the shape of the value.
program matmul_15
implicit none
real, allocatable :: x(:), w(:,:), b(:), z(:), a(:,:), c(:,:), d(:,:)
integer, allocatable :: iz(:)
complex, allocatable :: cz(:)
allocate(x(3), w(3,2), b(2))
x = [1, 2, 3]
w = reshape([1, 2, 3, 4, 5, 6], [3, 2])
b = [10, 20]

z = matmul(x, w) + b
if (size(z) /= 2 .or. lbound(z, 1) /= 1) error stop 1
if (any(z /= [24, 52])) error stop 2
deallocate(z)
allocate(z(0:4))
z = matmul(x, w) + b
if (size(z) /= 2 .or. lbound(z, 1) /= 1) error stop 3
if (any(z /= [24, 52])) error stop 4
deallocate(z)
allocate(z(-1:0))
z = matmul(x, w) + b
if (lbound(z, 1) /= -1 .or. any(z /= [24, 52])) error stop 5

iz = matmul(int(x), int(w)) - int(b)
if (size(iz) /= 2 .or. any(iz /= [4, 12])) error stop 6
cz = matmul(cmplx(x), cmplx(w)) + cmplx(b)
if (size(cz) /= 2 .or. any(cz /= cmplx([24, 52]))) error stop 7

a = transpose(w)
allocate(d(5,5))
d = matmul(a, w) + 1
if (any(shape(d) /= [2, 2])) error stop 8
if (any(d(:,1) /= [15, 33]) .or. any(d(:,2) /= [33, 78])) error stop 9
allocate(c(1,1))
c = matmul(w, a) * 0 + matmul(w, a)
if (any(shape(c) /= [3, 3]) .or. c(3,3) /= 45) error stop 10
z = matmul(a, x) + b
if (size(z) /= 2 .or. any(z /= [24, 52])) error stop 11
print *, "ok"
end program
