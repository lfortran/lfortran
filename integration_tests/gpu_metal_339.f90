program gpu_metal_339
! `reduce` on an offloaded `do concurrent`. Every thread accumulates into a
! slot of its own -- a device has no lock to serialise a shared one with --
! and the host folds the slots afterwards. Each accumulator starts at the
! identity of its operator, so a thread whose body never reaches it
! contributes nothing, and the fold starts from whatever the scalar already
! held, which is what a Fortran reduction accumulates onto.
implicit none
integer, parameter :: n = 1000
real :: a(n), s, p, lo, hi
integer :: ia(n), isum
integer :: i, j
real :: m(20, 30), msum

do i = 1, n
    a(i) = real(i)
    ia(i) = i
end do

! sum, onto a scalar that already holds something
s = 100.0
do concurrent (i = 1:n) reduce(+:s)
    s = s + a(i)
end do
if (abs(s - (100.0 + 500500.0)) > 1.0) error stop "sum"

! product over a short range, so the result stays finite
p = 1.0
do concurrent (i = 1:10) reduce(*:p)
    p = p * a(i)
end do
if (abs(p - 3628800.0) > 1.0) error stop "product"

! min and max
lo = 1.0e30
hi = -1.0e30
do concurrent (i = 1:n) reduce(min:lo) reduce(max:hi)
    lo = min(lo, a(i))
    hi = max(hi, a(i))
end do
if (abs(lo - 1.0) > 1.0e-6) error stop "min"
if (abs(hi - real(n)) > 1.0e-6) error stop "max"

! integer sum, and a body that only sometimes reaches the accumulator
isum = 0
do concurrent (i = 1:n) reduce(+:isum)
    if (mod(ia(i), 2) == 0) isum = isum + 1
end do
if (isum /= n / 2) error stop "conditional sum"

! iand/ior/ieor reductions are accepted too, but the gpu emitter
! writes those intrinsics as `(x ? y)` today, so a kernel using one
! does not compile whether it reduces or not. Left out until that
! is fixed rather than testing someone else's bug here.

! a rank-2 nest, so the slot index survives the index recovery
do j = 1, 30
    do i = 1, 20
        m(i, j) = real(i + j)
    end do
end do
msum = 0.0
do concurrent (i = 1:20, j = 1:30) reduce(+:msum)
    msum = msum + m(i, j)
end do
if (abs(msum - 15600.0) > 1.0) error stop "rank-2 sum"

print *, s, p, lo, hi, isum, msum
print *, "PASS"
end program
