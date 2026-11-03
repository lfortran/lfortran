! The loop bounds a kernel takes as parameters when its launch may run in
! chunks (lfortran/lfortran#16569). A chunk sets the bounds of the
! dimensions it splits, so those are parameters even where they are
! constants. A dimension with a constant extent, which a chunk covers
! whole wherever it is numbered, keeps constant bounds, and a nest that
! cannot have more iterations than a launch runs is never split.
program gpu_kernel_source_41
implicit none
integer :: n, m, i, j, k, c
real :: x(1000), w(0:1000), y(64, 100), v(100, 3), z(4, 5, 6)
n = 1000
m = 100
! An integer(4) index from 1 takes at most huge(0) values: the launch is
! never split, and the start of `i` stays a constant.
do concurrent (i = 1:n)
    x(i) = i
end do
! From 0 it can take huge(0) + 1: both bounds of `i` are parameters.
do concurrent (i = 0:n)
    w(i) = i
end do
! A constant extent of the fastest dimension, `i`, which a chunk covers
! whole, and a run-time one of `j`: only the bounds of `j` are parameters.
do concurrent (i = 1:64, j = 1:m)
    y(i, j) = i + j
end do
! A run-time extent of the fastest dimension, `i`, and a constant one of
! `c`, numbered after it, which a chunk still covers whole: the bounds of
! `c` stay constants, and the kernel divides by its constant extent.
do concurrent (i = 1:m, c = 1:3)
    v(i, c) = i * c
end do
! Constant extents that fit in one launch: no bound is a parameter.
do concurrent (i = 1:4, j = 1:5, k = 1:6)
    z(i, j, k) = i + j + k
end do
if (abs(sum(x) - 500500) > 1e-3) error stop 1
if (abs(sum(w) - 500500) > 1e-3) error stop 2
if (abs(sum(y) - (100 * 2080 + 64 * 5050)) > 1e-1) error stop 3
if (abs(sum(v) - 6 * 5050) > 1e-1) error stop 4
if (abs(sum(z) - 1080) > 1e-3) error stop 5
print *, sum(x), sum(w), sum(y), sum(v), sum(z)
end program
