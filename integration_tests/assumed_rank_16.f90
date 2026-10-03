! An assumed-rank dummy argument that is host-associated into a contained
! procedure gets hoisted into a nested-variable context module by the
! nested_vars pass. The hoisted variable has to keep its assumed-rank array
! type; collapsing it to the bare element type used to make the `select rank`
! inside the contained procedure fail with
! "Cannot extract the physical type of 2 type."
! See https://github.com/lfortran/lfortran/issues/12683
module assumed_rank_16_mod
   implicit none
contains

   subroutine elementcopy(src, dst)
      real, intent(in) :: src(..)
      real :: dst(..)
      select rank (src)
      rank (1)
         call step2(src)
      end select
   contains

      ! `dst` is only reachable here through host association, so the pass
      ! has to move it into the nested context with its type intact.
      subroutine step2(s)
         real, intent(in) :: s(:)
         select rank (dst)
         rank (1)
            call ecopy(s, dst)
         rank (2)
            call ecopy(s, dst(:, 1))
         end select
      end subroutine step2

      subroutine ecopy(a, b)
         real, intent(in) :: a(:)
         real :: b(:)
         integer :: i
         do i = 1, size(a)
            b(i) = 2.0 * a(i)
         end do
      end subroutine ecopy

   end subroutine elementcopy

end module assumed_rank_16_mod

program assumed_rank_16
   use assumed_rank_16_mod
   implicit none

   real :: x(4)
   real :: y(4)
   real :: z(4, 2)
   integer :: i

   x = [1.0, 2.0, 3.0, 4.0]

   y = 0.0
   call elementcopy(x, y)
   print *, y
   do i = 1, 4
      if (abs(y(i) - 2.0 * x(i)) > 1.0e-6) error stop "rank 1 dst"
   end do

   z = 0.0
   call elementcopy(x, z)
   print *, z(:, 1)
   do i = 1, 4
      if (abs(z(i, 1) - 2.0 * x(i)) > 1.0e-6) error stop "rank 2 dst"
      if (abs(z(i, 2)) > 1.0e-6) error stop "rank 2 dst untouched column"
   end do

end program assumed_rank_16
