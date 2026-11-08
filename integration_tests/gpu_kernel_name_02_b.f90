! The subroutine `foo` bound to `gpu_kernel_name_02_b`; the one in
! gpu_kernel_name_02_a.f90 has the same Fortran name.
subroutine foo(n, b) bind(c, name="gpu_kernel_name_02_b")
use iso_c_binding, only: c_int
implicit none
integer(c_int), value :: n
integer(c_int), intent(out) :: b(n)
integer :: i
do concurrent (i = 1:n)
    b(i) = 2 * i
end do
end subroutine
