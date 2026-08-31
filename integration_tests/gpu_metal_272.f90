! A run-time sized local of a spliced device callee becomes a per-thread
! workspace the host has to size before it launches the kernel.  When the
! actual argument is a function call -- `mulop(self, e(col, cols))` -- the
! callee's `size(arr)` becomes `size(e(col, cols))`, and the extent walk
! gave up as soon as the array expression was not a plain variable, so
! every such loop stayed on the host.
!
! An expression that is not a designator still carries its own shape in
! its type: a result declared `real :: r(length)` records `length` as the
! extent of its one dimension, and semantics has already rewritten that
! in terms of the actual arguments -- caller-scope symbols, which is what
! the host can evaluate.
module gpu_metal_272_m
   implicit none

   type :: op_t
      integer :: m_
      real, allocatable :: upper_(:,:)
   end type op_t

contains

   ! The local `work` is sized from `size(arr)`, so it becomes the
   ! workspace whose extent the host has to resolve.
   pure function mulop(self, arr) result(r)
      type(op_t), intent(in) :: self
      real, intent(in) :: arr(:)
      real :: r(self%m_ + 1)
      real :: work(size(arr) - 1)
      integer :: k
      do k = 1, size(arr) - 1
         work(k) = arr(k) + 2.0*arr(k + 1)
      end do
      r = 0.0
      do k = 1, min(self%m_ + 1, size(work))
         r(k) = work(k)
      end do
   end function mulop

   ! The `dir`-th column of the identity matrix.  Its extent is the
   ! actual `length`, which is what makes `size(e(...))` resolvable.
   pure function e(dir, length) result(unit_vector)
      integer, intent(in) :: dir, length
      real :: unit_vector(length)
      unit_vector = 0.0
      unit_vector(dir) = 1.0
   end function e

   ! A function call as the actual argument.
   subroutine assemble(self, g)
      type(op_t), intent(in) :: self
      real, intent(out) :: g(:,:)
      integer :: col, cols
      cols = self%m_ + 2
      do concurrent (col = 1:cols)
         g(:,col) = mulop(self, e(col, cols))
      end do
   end subroutine assemble

   ! The fence: a plain array variable as the actual argument, which
   ! resolved before this change and must keep offloading.
   subroutine assemble_var(self, cin, g)
      type(op_t), intent(in) :: self
      real, intent(in) :: cin(:,:)
      real, intent(out) :: g(:,:)
      integer :: col
      do concurrent (col = 1:size(cin, 2))
         g(:,col) = mulop(self, cin(:,col))
      end do
   end subroutine assemble_var

end module gpu_metal_272_m

program gpu_metal_272
   use gpu_metal_272_m
   implicit none
   integer, parameter :: mm = 3, rows = mm + 1, cols = mm + 2
   type(op_t) :: s
   real :: g(rows, cols), cin(cols, cols)
   real :: expected(rows, cols)
   real :: col_in(cols)
   integer :: col, k

   s%m_ = mm
   allocate(s%upper_(2, 2))

   ! Reference values, computed on the host from the same definitions.
   do col = 1, cols
      col_in = 0.0
      col_in(col) = 1.0
      cin(:, col) = col_in
      expected(:, col) = 0.0
      do k = 1, min(rows, cols - 1)
         expected(k, col) = col_in(k) + 2.0*col_in(k + 1)
      end do
   end do

   g = -1.0
   call assemble(s, g)
   do col = 1, cols
      print *, g(:, col)
      do k = 1, rows
         if (abs(g(k, col) - expected(k, col)) > 1.0e-4) then
            error stop "assemble"
         end if
      end do
   end do

   g = -1.0
   call assemble_var(s, cin, g)
   do col = 1, cols
      do k = 1, rows
         if (abs(g(k, col) - expected(k, col)) > 1.0e-4) then
            error stop "assemble_var"
         end if
      end do
   end do

   print *, "ok"
end program gpu_metal_272
