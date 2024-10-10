program main
    use omp_lib
! declare variables as required
    integer :: ny, nx, nz  
    integer :: iy, ix, iz
    ny=3
    nx=4
    nz=5

!$omp parallel do collpase(3) private(iy, ix, iz)
do iy = 1, ny
    do ix = 1, nx
        do iz = 1, nz
            print *,"iy->", iy,"ix->", ix,"iz->", iz
        end do
    end do
end do
!$omp end parallel do

end program

! Converted to 

! !$omp parallel do private(iy, ix, iz)
! do I = 0, (ny * nx * nz) - 1
!     iy = (I / (nx * nz)) + 1
!     ix = ((I / nz) % nx) + 1
!     iz = (I % nz) + 1
!     print *, "iy->", iy, "ix->", ix, "iz->", iz
! end do
! !$omp end parallel do