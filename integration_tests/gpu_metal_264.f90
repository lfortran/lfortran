! Fence for the run-time sized array-constructor result handled in
! gpu_metal_263: a constructor whose extent is a compile-time constant
! still becomes an ordinary fixed-size local of the device function, and
! the loop still becomes a kernel.
module gpu_metal_264_m
   implicit none
contains
   pure function corners(c) result(ends)
      real, intent(in) :: c(:)
      real :: ends(3)
      integer, parameter :: end_point = 1
      ends = [ c(end_point), 0.5*(c(end_point) + c(size(c))), c(size(c)) ]
   end function corners
end module gpu_metal_264_m

program gpu_metal_264
   use gpu_metal_264_m
   implicit none
   integer, parameter :: n = 5, m = 4
   integer :: i, j
   real :: a(n, m), out(3, m)
   do j = 1, m
      do i = 1, n
         a(i, j) = real(i + 10*j)
      end do
   end do
   out = 0
   do concurrent (j = 1:m)
      out(:, j) = corners(a(:, j))
   end do
   do j = 1, m
      print *, out(:, j)
      if (abs(out(1, j) - a(1, j)) > 1.0e-5) error stop "corners lo"
      if (abs(out(2, j) - 0.5*(a(1, j) + a(n, j))) > 1.0e-5) error stop "corners mid"
      if (abs(out(3, j) - a(n, j)) > 1.0e-5) error stop "corners hi"
   end do
end program gpu_metal_264
