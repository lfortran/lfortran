program gpu_kernel_source_43
! Which arrays an assignment to a section makes private to each iteration
! of a do concurrent loop on the GPU (lfortran/lfortran#17013): a section
! over the full extent defines the whole array, so `full` and `grid` are
! locals of the kernel; a section over part of it does not, so `head`,
! `tail`, `odd` and `col` stay kernel arguments, as do `lo` and `sz`,
! whose definition reads the array in the bounds of a section.
implicit none
integer, parameter :: n = 8
integer :: full(4), grid(3, 2), head(4), tail(4), odd(4), col(3, 2)
integer :: lo(4), sz(4), out(n), i

do concurrent (i = 1:n)
    full(:) = i
    grid(1:3, :) = 2 * i
    out(i) = full(mod(i, 4) + 1) + grid(mod(i, 3) + 1, mod(i, 2) + 1)
end do
do i = 1, n
    if (out(i) /= 3 * i) error stop 1
end do

do concurrent (i = 1:n)
    head(1:3) = i
    out(i) = head(mod(i, 3) + 1)
end do
do i = 1, n
    if (out(i) /= i) error stop 2
end do

do concurrent (i = 1:n)
    tail(2:) = i
    out(i) = tail(mod(i, 3) + 2)
end do
do i = 1, n
    if (out(i) /= i) error stop 3
end do

do concurrent (i = 1:n)
    odd(::2) = i
    out(i) = odd(2 * mod(i, 2) + 1)
end do
do i = 1, n
    if (out(i) /= i) error stop 4
end do

do concurrent (i = 1:n)
    col(:, 2) = i
    out(i) = col(mod(i, 3) + 1, 2)
end do
do i = 1, n
    if (out(i) /= i) error stop 5
end do

lo = 1
sz = 1
do concurrent (i = 1:1)
    lo(lo(4):4) = 7
    sz = ubound(sz(sz(1):4), 1)
    out(i) = lo(4) + sz(4)
end do
if (out(1) /= 11) error stop 6
print *, "ok"
end program
