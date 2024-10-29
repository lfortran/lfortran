program openmp_39
    use omp_lib
! declare variables as required
    integer :: ny, nx, nz,nk  
    integer :: iy, ix, iz,ik
    ny=3
    nx=4
    nz=5
    nk=6

!$omp parallel do collapse(2) private(iy, ix, iz)
do ix = 1, nx
    do iy = 1, ny
        print *,"iy->", iy,"ix->", ix
        do iz = 1, nz
            do ik = 1, nk
            print *,"iy->", iy,"ix->", ix,"iz->", iz,"ik->", ik
            end do
        end do
    end do
end do
!$omp end parallel do

end program
