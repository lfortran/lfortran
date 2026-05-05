module derived_types_139_mod
   implicit none
contains
   subroutine sub_a(total)
      integer, intent(out) :: total
      type :: idx_type
         integer, allocatable, dimension(:) :: loc
      end type idx_type
      type(idx_type), dimension(3) :: idx
      integer :: i, j
      total = 0
      do i = 1, 3
         allocate(idx(i)%loc(5))
         do j = 1, 5
            idx(i)%loc(j) = j
         end do
         total = total + size(idx(i)%loc)
      end do
   end subroutine sub_a

   subroutine sub_b(total)
      integer, intent(out) :: total
      type :: idx_type
         integer, allocatable, dimension(:) :: loc
      end type idx_type
      type(idx_type), dimension(5) :: idx
      integer :: i, j
      total = 0
      do i = 1, 5
         allocate(idx(i)%loc(7))
         do j = 1, 7
            idx(i)%loc(j) = j
         end do
         total = total + size(idx(i)%loc)
      end do
   end subroutine sub_b
end module derived_types_139_mod

program derived_types_139
   use derived_types_139_mod
   implicit none
   integer :: a_total, b_total

   call sub_a(a_total)
   if (a_total /= 15) error stop "wrong total in sub_a"

   call sub_b(b_total)
   if (b_total /= 35) error stop "wrong total in sub_b"

   print *, "ok"
end program derived_types_139
