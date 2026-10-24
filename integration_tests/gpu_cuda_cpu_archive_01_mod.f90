module gpu_cuda_cpu_archive_01_m
    implicit none
contains
    subroutine twice(x, y)
        real, intent(in) :: x(4)
        real, intent(out) :: y(4)
        integer :: i
        do concurrent (i = 1:4)
            y(i) = 2.0*x(i)
        end do
    end subroutine
end module
