! Two external subroutines with the same Fortran name `foo` but distinct
! binding labels, each in a file of its own with a kernel of its own
! (lfortran/lfortran#17143). Only a binding label is a global identifier,
! so a kernel named after `foo` would have the same name in both files:
! Metal rejected the second kernel, and the CUDA objects failed to link
! with duplicate symbols.
program gpu_kernel_name_02
use iso_c_binding, only: c_int
implicit none
interface
    subroutine foo_a(n, a) bind(c, name="gpu_kernel_name_02_a")
    import :: c_int
    integer(c_int), value :: n
    integer(c_int), intent(out) :: a(n)
    end subroutine
    subroutine foo_b(n, b) bind(c, name="gpu_kernel_name_02_b")
    import :: c_int
    integer(c_int), value :: n
    integer(c_int), intent(out) :: b(n)
    end subroutine
end interface
integer(c_int), parameter :: n = 1000
integer(c_int) :: a(n), b(n)
integer :: i
call foo_a(n, a)
call foo_b(n, b)
do i = 1, n
    if (a(i) /= i + 1) error stop 1
    if (b(i) /= 2 * i) error stop 2
end do
print *, a(n), b(n)
end program
