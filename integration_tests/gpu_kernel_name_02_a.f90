! The subroutine `foo` bound to `gpu_kernel_name_02_a`; the one in
! gpu_kernel_name_02_b.f90 has the same Fortran name.
subroutine foo(n, a) bind(c, name="gpu_kernel_name_02_a")
use iso_c_binding, only: c_int
implicit none
integer(c_int), value :: n
integer(c_int), intent(out) :: a(n)
integer :: i
do concurrent (i = 1:n)
    a(i) = i + 1
end do
end subroutine
