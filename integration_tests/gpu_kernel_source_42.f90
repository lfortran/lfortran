! The loop bounds a kernel takes as parameters when its launch may run in
! chunks of at most 50 iterations (lfortran/lfortran#16569), as this test
! is compiled with --gpu-chunk-limit=50. The 10 iterations of `i` fit in a
! chunk, so its bounds stay constants; 100 of `i` and `j` do not, so a
! chunk splits `j`, and then `k`: their bounds are parameters. In the
! second nest the 4 values of `k` fit beside those of `i`, so a chunk
! covers `k` whole, and its bounds stay constants too.
program gpu_kernel_source_42
implicit none
integer :: i, j, k
integer :: a(10, 10, 10), b(10, 10, 4)
a = 0
do concurrent (i = 1:10, j = 1:10, k = 1:10)
    a(i, j, k) = 100 * i + 10 * j + k
end do
b = 0
do concurrent (i = 1:10, j = 1:10, k = 1:4)
    b(i, j, k) = 100 * i + 10 * j + k
end do
do k = 1, 10
    do j = 1, 10
        do i = 1, 10
            if (a(i, j, k) /= 100 * i + 10 * j + k) error stop 1
            if (k <= 4) then
                if (b(i, j, k) /= 100 * i + 10 * j + k) error stop 2
            end if
        end do
    end do
end do
print *, "ok"
end program
