program gpu_unroll_runtime_start_01
! A sequential loop of device code whose start is only known at run time,
! as over the entries of a row of a sparse matrix, of 0 to 7 iterations.
! The values are small integers, so each sum is exact in single precision.
implicit none
integer, parameter :: nr = 1000
integer :: rowptr(nr + 1), col(4*nr), i, k, nz
real :: val(4*nr), x(nr), y(nr), t, expected

nz = 0
do i = 1, nr
    rowptr(i) = nz + 1
    do k = 1, mod(i, 8)
        if (k > 4) exit
        nz = nz + 1
        col(nz) = mod(i*k, nr) + 1
        val(nz) = k
    end do
end do
rowptr(nr + 1) = nz + 1
do i = 1, nr
    x(i) = i
end do

do concurrent (i = 1:nr) local(t, k)
    t = 0
    do k = rowptr(i), rowptr(i + 1) - 1
        t = t + val(k)*x(col(k))
    end do
    y(i) = t
end do

do i = 1, nr
    expected = 0
    do k = rowptr(i), rowptr(i + 1) - 1
        expected = expected + val(k)*x(col(k))
    end do
    if (y(i) /= expected) error stop 1
end do

! A start of -3, below the batch size.
do concurrent (i = 1:nr) local(t, k)
    t = 0
    do k = -3 + mod(i, 2), mod(i, 9) - 3
        t = t + k
    end do
    y(i) = t
end do
do i = 1, nr
    expected = 0
    do k = -3 + mod(i, 2), mod(i, 9) - 3
        expected = expected + k
    end do
    if (y(i) /= expected) error stop 2
end do
print *, sum(y)
end program
