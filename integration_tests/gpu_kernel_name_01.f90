! The kernels of separately compiled files linked into one program have
! distinct names (lfortran/lfortran#17143): the first kernel of each file
! used to be named `__lfortran_gpu_kernel_0` in both, so Metal ran the
! kernel of this file for the external subroutine, and the CUDA objects
! failed to link with duplicate symbols.
program gpu_kernel_name_01
implicit none
interface
    subroutine gpu_kernel_name_01_ext(n, b)
    integer, intent(in) :: n
    integer, intent(out) :: b(n)
    end subroutine
end interface
integer, parameter :: n = 1000
integer :: a(n), b(n), i
do concurrent (i = 1:n)
    a(i) = i + 1
end do
call gpu_kernel_name_01_ext(n, b)
do i = 1, n
    if (a(i) /= i + 1) error stop 1
    if (b(i) /= 2 * i) error stop 2
end do
print *, a(n), b(n)
end program
