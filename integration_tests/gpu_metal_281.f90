! A named constant is declared wherever it is scoped, and every reference
! to it renders as its name.  Nothing ever assigns to one, so a Metal
! declaration without the initialiser -- `int bump;` -- left each thread
! reading whatever its stack happened to hold.  The constants of a kernel
! and of a device function were already initialised; the ones declared in
! the block a spliced callee creates were not, and a bound taken from such
! a constant sent the shader off the end of its buffers.
program gpu_metal_281
   use gpu_metal_281_m
   implicit none
   integer, parameter :: n = 5, m = 4
   real :: a(n, m), r(n, m)
   real :: expected
   integer :: i, j

   do j = 1, m
      do i = 1, n
         a(i, j) = real(i + 10*j)
      end do
   end do

   r = 0
   do concurrent (j = 1:m)
      r(:, j) = shift(a(:, j))
   end do

   do j = 1, m
      print *, r(:, j)
      do i = 1, 2
         if (abs(r(i, j) - a(i, j)) > 1.0e-4) error stop "head"
      end do
      do i = 3, n
         expected = 2.0*a(i, j) + real(n - 3)
         if (abs(r(i, j) - expected) > 1.0e-4) error stop "tail"
      end do
   end do

end program gpu_metal_281
