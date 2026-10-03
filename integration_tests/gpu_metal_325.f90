! A `do concurrent` nested in a BLOCK of an offloaded body is not fused
! into the outer kernel. It is sequentialized on the device, which
! `--gpu-offload-report` records as `status=device-serial` with
! `reason=sequentialized-for-device`.
program gpu_metal_325
implicit none
integer :: i, j, ii, jj
real :: a(3, 4)

a = 0.0
do concurrent (i = 1:3)
    block
        do concurrent (j = 1:4)
            a(i, j) = real(10 * i + j)
        end do
    end block
end do

do ii = 1, 3
    do jj = 1, 4
        if (abs(a(ii, jj) - real(10 * ii + jj)) > 1.0e-5) error stop
    end do
end do

print *, "PASS"
end program
