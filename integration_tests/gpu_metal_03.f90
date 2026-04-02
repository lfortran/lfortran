program gpu_metal_03
! 3D do concurrent: volume initialization
implicit none
integer, parameter :: nx = 32, ny = 32, nz = 32
real :: vol(nx, ny, nz), vol_expected(nx, ny, nz)
integer :: i, j, k

do k = 1, nz
    do j = 1, ny
        do i = 1, nx
            vol_expected(i, j, k) = real(i) + real(j) * 2.0 + real(k) * 3.0
        end do
    end do
end do

do concurrent (i = 1:nx, j = 1:ny, k = 1:nz)
    vol(i, j, k) = real(i) + real(j) * 2.0 + real(k) * 3.0
end do

do k = 1, nz
    do j = 1, ny
        do i = 1, nx
            if (abs(vol(i, j, k) - vol_expected(i, j, k)) > 1.0e-5) error stop
        end do
    end do
end do

print *, "PASSED"
end program
