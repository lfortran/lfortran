program openmp_38
    use omp_lib
! declare variables as required
    integer :: ny, nx, nz  
    integer :: iy, ix, iz
    ny=3
    nx=4
    nz=5

!$omp parallel do collapse(3) private(iy, ix, iz)
do iy = 1, ny
    do ix = 1, nx
        do iz = 1, nz
            print *,"iy->", iy,"ix->", ix,"iz->", iz
        end do
    end do
end do
!$omp end parallel do

end program
