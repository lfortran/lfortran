! A device callee spliced into a kernel is copied verbatim from wherever
! its body was found.  When the callee is a submodule procedure compiled
! on its own, the kernel builder reads that body back from the `.smod`
! file -- a body the implied-do lowering never saw, because that pass ran
! over this translation unit before the submodule was loaded.  The array
! constructor therefore reached the kernel with its implied-do still
! standing: the temporary extraction that runs afterwards hoisted the
! loop-variant element out of it, evaluating it once instead of once per
! iteration, and the Metal code generator, having no rendering for an
! implied-do, emitted a shader that would not compile.  A loaded body is
! now lowered as soon as it is read, so it is in the same shape as one
! compiled alongside its caller.
program gpu_metal_269
   use gpu_metal_269_m
   implicit none
   integer, parameter :: n = 6, m = 4
   real :: a(n, m), r(n, m)
   real :: expected
   integer :: i, j
   type(weights_t) :: w

   allocate(w%pair(2))
   w%pair = [1.0, 2.0]

   do j = 1, m
      do i = 1, n
         a(i, j) = real(i + 10*j)
      end do
   end do

   ! Run-time trip count: the implied-do runs n-2 times.
   r = 0
   do concurrent (j = 1:m)
      r(:, j) = ramp(a(:, j))
   end do
   do j = 1, m
      print *, r(:, j)
      if (abs(r(1, j) - 2.0*a(1, j)) > 1.0e-4) error stop "ramp head"
      do i = 2, n - 1
         expected = a(i, j) + 0.5*a(i + 1, j)
         if (abs(r(i, j) - expected) > 1.0e-4) error stop "ramp body"
      end do
      if (abs(r(n, j) - 3.0*a(n, j)) > 1.0e-4) error stop "ramp tail"
   end do

   ! A dot_product over a section of the dummy inside the implied-do:
   ! the element is loop variant, so hoisting it out of the constructor
   ! would give every slot the same value.
   r = 0
   do concurrent (j = 1:m)
      r(:, j) = paired(w, a(:, j))
   end do
   do j = 1, m
      print *, r(:, j)
      if (abs(r(1, j) - 2.0*a(1, j)) > 1.0e-4) error stop "paired head"
      do i = 2, n - 1
         expected = 1.0*a(i, j) + 2.0*a(i + 1, j)
         if (abs(r(i, j) - expected) > 1.0e-4) error stop "paired body"
      end do
      if (abs(r(n, j) - 3.0*a(n, j)) > 1.0e-4) error stop "paired tail"
   end do

   ! A constant trip count as the fence.
   r = 0
   do concurrent (j = 1:m)
      r(:, j) = fixed_head(a(:, j))
   end do
   do j = 1, m
      print *, r(:, j)
      do i = 1, 3
         if (abs(r(i, j) - 10.0*a(i, j)) > 1.0e-4) error stop "fixed head"
      end do
      do i = 4, n
         if (abs(r(i, j) - a(i, j)) > 1.0e-4) error stop "fixed tail"
      end do
   end do

end program gpu_metal_269
