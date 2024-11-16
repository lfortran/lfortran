program openmp_37
    use omp_lib
 ! declare variables as required
    integer :: ny, nx, nk
    integer :: iy, ix, ik
    ny=3
    nx=4
    nk=6
 
 !$omp parallel do private(iy, ix)
    do iy = 1, ny
 
     !$omp parallel do collapse(2)
       do ix = 1, nx
          do ik = 1, nk
             print *,"iy->", iy,"ix->", ix
          end do
       end do
     !$omp end parallel do
 
     !$omp parallel do
       do ix = 1, nx
          do ik = 1, nk
             print *,"iy->", iy,"ix->", ix
          end do
       end do
     !$omp end parallel do
       
    end do
 !$omp end parallel do
 
 end program