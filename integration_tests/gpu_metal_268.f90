! `ubound(x, d)` names one dimension.  For an allocatable array component
! of an element of an array of derived type, the Metal code generator
! dropped the dimension and emitted the element count of the whole
! component instead, so a `matmul` over such a component bounded both of
! its loops by rows*cols: it wrote past the end of the result and summed
! the wrong products.  The loops here are the ones a kernel built from
! `matmul(ops(sel)%w, ...)` runs.
program gpu_metal_268
   use gpu_metal_268_m
   implicit none
   integer, parameter :: rows = 2, cols = 3, m = 4
   real :: a(cols, m), r(rows, m)
   real :: expected
   integer :: i, j, k
   type(op_t) :: ops(2)

   ! The unused element is deliberately a different shape, so an index
   ! taken from the wrong element shows up too.
   allocate(ops(1)%w(1, 1))
   ops(1)%w = 0

   allocate(ops(sel)%w(rows, cols))
   do j = 1, cols
      do i = 1, rows
         ops(sel)%w(i, j) = real(i) + 0.25*real(j)
      end do
   end do

   do j = 1, m
      do i = 1, cols
         a(i, j) = real(i + 10*j)
      end do
   end do

   r = 0
   do concurrent (j = 1:m)
      r(:, j) = matmul(ops(sel)%w, a(:, j))
   end do

   do j = 1, m
      print *, r(:, j)
      do i = 1, rows
         expected = 0
         do k = 1, cols
            expected = expected + ops(sel)%w(i, k)*a(k, j)
         end do
         if (abs(r(i, j) - expected) > 1.0e-3) error stop "matmul"
      end do
   end do

end program gpu_metal_268
