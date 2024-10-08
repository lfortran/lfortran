program main
    use omp_lib
! declare variables as required
    integer :: ny, nx, nz  
    integer :: iy, ix, iz
    ny=3
    nx=4
    nz=5

!$omp parallel do collapse(2) private(iy, ix, iz)
do iy = 1, ny
    do ix = 1, nx
        print *,"iy->", iy,"ix->", ix
    end do
    do iz = 1, nz
        print *,"iy->", iy,"iz->", iz
    end do
end do
!$omp end parallel do

end program

! Converted to 

! !$omp parallel do private(iy, ix, iz)
! do I = 0, (ny * nx) - 1
!     iy = (I / nx) + 1
!     ix = (I % nx) + 1
!     print *, "iy->", iy, "ix->", ix
! end do
! !$omp parallel do private(iy, ix, iz)
! do I = 0, (nz * nx) - 1
!     iz = (I / nx) + 1
!     ix = (I % nx) + 1
!     print *, "iz->", iz, "ix->", ix
! end do
! !$omp end parallel do