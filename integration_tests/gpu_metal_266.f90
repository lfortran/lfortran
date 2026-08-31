! Splicing a device callee substitutes the actual argument for the dummy,
! so a callee that sections its own dummy -- `c(1:k)` -- over an actual
! that is itself a section left a section of a section behind.  A device
! pointer carries a base and a count and nothing else, so the inner
! section's stride would simply be dropped, and the loop was declined
! (`nested-section-cannot-be-addressed`).  The sectioned actual is now
! copied into a contiguous array of the spliced block first, and the dummy
! stands for that.
module gpu_metal_266_m
   implicit none
contains
   ! The automatic array `w` is what makes this callee one that has to be
   ! spliced into the kernel rather than emitted as a device function.
   pure function head_and_tail(c, k) result(f)
      real, intent(in) :: c(:)
      integer, intent(in) :: k
      real, allocatable :: f(:)
      real :: w(size(c))
      integer :: p
      allocate(f(size(c)))
      w = c
      f(1) = sum(c(1:k))
      do p = 2, size(w)
         f(p) = c(p) - w(1)
      end do
   end function head_and_tail
end module gpu_metal_266_m

program gpu_metal_266
   use gpu_metal_266_m
   implicit none
   integer, parameter :: n = 4, m = 3
   real :: a(n, m), col(n, m), row(m, n)
   integer :: i, j

   do j = 1, m
      do i = 1, n
         a(i, j) = real(i + 10*j)
      end do
   end do

   ! A row section is not contiguous: its stride is what the copy has to
   ! preserve, and a base pointer with a count alone would read the wrong
   ! run of elements.  Four rows with distinct values, so a stride taken
   ! from the wrong dimension shows up as a wrong number.
   row = 0
   do concurrent (i = 1:n)
      row(:, i) = head_and_tail(a(i, :), 2)
   end do
   do i = 1, n
      print *, row(:, i)
      if (abs(row(1, i) - (a(i, 1) + a(i, 2))) > 1.0e-4) error stop "row head"
      do j = 2, m
         if (abs(row(j, i) - (a(i, j) - a(i, 1))) > 1.0e-5) error stop "row tail"
      end do
   end do

   ! A contiguous column section as the fence.
   col = 0
   do concurrent (j = 1:m)
      col(:, j) = head_and_tail(a(:, j), 2)
   end do
   do j = 1, m
      print *, col(:, j)
      if (abs(col(1, j) - (a(1, j) + a(2, j))) > 1.0e-4) error stop "col head"
      do i = 2, n
         if (abs(col(i, j) - (a(i, j) - a(1, j))) > 1.0e-5) error stop "col tail"
      end do
   end do
end program gpu_metal_266
