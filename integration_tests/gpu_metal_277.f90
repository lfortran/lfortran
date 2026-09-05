! An allocatable function result built from an array constructor whose
! implied-do has a run-time trip count, called from a `do concurrent`.
! The constructor becomes a run-time sized local of the callee, which a
! Metal device function has no declaration for; the loop has to either
! splice the callee into the kernel or stay on the host, never emit a
! shader that cannot be loaded.
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
end module gpu_metal_277_m

program gpu_metal_277
   use gpu_metal_277_m
   implicit none
   integer, parameter :: n = 6, m = 3
   integer :: i, j
   real :: w(2), c(n), out(n + 1, m), expected
   w = [0.5, 0.5]
   do i = 1, n
      c(i) = real(i)
   end do
   out = 0
   do concurrent (j = 1:m)
      out(:, j) = face_values(w, c)*real(j)
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
end program gpu_metal_277
