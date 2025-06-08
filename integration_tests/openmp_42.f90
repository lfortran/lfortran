program openmp_42
    use omp_lib
! declare variables as required
    integer :: ny, nx, nz  , i,j
    integer :: iy, ix, iz
    ny=3
    nx=4
    nz=5
do j=1,2
    do i =1,3
    !$omp parallel do collapse(2) private(iy, ix, iz)
    do iy = 1, ny
        do ix = 1, nx
            do iz = 1, nz
                print *,"iy->", iy,"ix->", ix,"iz->", iz
            end do
        end do
    end do
    !$omp end parallel do
    end do
end do
end program