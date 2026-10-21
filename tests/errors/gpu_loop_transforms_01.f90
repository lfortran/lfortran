! `--gpu-loop-transforms` accepts only the names of the transformations.
program gpu_loop_transforms_01
    implicit none
    integer :: i
    real :: x(4)
    do concurrent (i = 1:4)
        x(i) = real(i)
    end do
    print *, x
end program
