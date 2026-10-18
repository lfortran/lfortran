! An allocatable function result built from an array constructor whose
! implied-do has a run-time trip count, called from a `do concurrent`.
! The constructor becomes a run-time sized local of the callee, which a
! Metal device function has no declaration for, so it becomes an argument
! the kernel gives the callee: the shader never declares it.
module gpu_metal_277_m
   implicit none
contains
   pure function face_values(w, c) result(faces)
      real, intent(in) :: w(:)
      real, intent(in) :: c(:)
      real, allocatable :: faces(:)
      integer :: row
      integer, parameter :: end_point = 1
      faces = [ c(end_point)*w(end_point) &
              , (dot_product(w, c(row:row + size(w) - 1)), &
                 row = end_point, size(c) - size(w) + 1) &
              , c(size(c))*w(end_point) ]
   end function face_values

   pure function pair_values(x) result(pair)
      real, intent(in) :: x
      real :: pair(2)
      pair = [x, 2*x]
   end function

   subroutine check_runtime(n)
      integer, intent(in) :: n
      real :: w(2), c(n), out(n + 1, 3), expected
      integer :: i, j
      w = [0.5, 0.5]
      do i = 1, n
         c(i) = real(i)
      end do
      do concurrent (j = 1:3)
         out(:, j) = face_values(w, c)*real(j)
      end do
      do j = 1, 3
         do i = 1, n + 1
            if (i == 1) then
               expected = 0.5*real(j)
            else if (i == n + 1) then
               expected = 0.5*real(n*j)
            else
               expected = (real(i) - 0.5)*real(j)
            end if
            if (abs(out(i, j) - expected) > 1.0e-5) error stop "runtime face_values"
         end do
      end do
   end subroutine
end module gpu_metal_277_m

program gpu_metal_277
   use gpu_metal_277_m
   implicit none
   integer, parameter :: n = 6, m = 3
   integer :: i, j
   real :: w(2), c(n), out(n + 1, m), pairs(2, m), expected
   w = [0.5, 0.5]
   do i = 1, n
      c(i) = real(i)
   end do
   out = 0
   do concurrent (j = 1:m)
      out(:, j) = face_values(w, c)*real(j)
      pairs(:, j) = pair_values(real(j))
   end do
   do j = 1, m
      print *, out(:, j)
      do i = 1, n + 1
         if (i == 1) then
            expected = c(1)*0.5*real(j)
         else if (i == n + 1) then
            expected = c(n)*0.5*real(j)
         else
            expected = 0.5*(c(i - 1) + c(i))*real(j)
         end if
         if (abs(out(i, j) - expected) > 1.0e-5) error stop "face_values"
      end do
   end do
   do j = 1, m
      if (any(abs(pairs(:,j) - [real(j), real(2*j)]) > 1.e-5)) error stop "thread pair"
   end do
   call check_runtime(n + 2)
end program gpu_metal_277
