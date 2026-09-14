module string_122_mod
   implicit none
   type :: holder
      character(len=4), allocatable :: names(:)
   end type holder
contains
   integer function len_lb0(a) result(r)
      character(len=*), intent(in) :: a(0:)
      character(len=len(a)) :: tmp
      if (size(a) /= 0) error stop
      if (len(tmp) /= len(a)) error stop
      r = len(a)
   end function len_lb0

   integer function len_1d(a) result(r)
      character(len=*), intent(in) :: a(:)
      r = len(a)
   end function len_1d

   integer function len_2d(a) result(r)
      character(len=*), intent(in) :: a(:,:)
      r = len(a)
   end function len_2d
end module string_122_mod

program string_122
   use string_122_mod
   implicit none
   character(len=5) :: empty(0)
   character(len=3) :: full(4)
   character(len=6) :: grid(0, 3)
   character(len=:), allocatable :: dal(:)
   type(holder) :: h

   if (len_lb0(empty) /= 5) error stop
   if (len_1d(empty) /= 5) error stop
   if (len_1d(full) /= 3) error stop
   if (len_2d(grid) /= 6) error stop

   allocate(character(len=7) :: dal(0))
   if (len(dal) /= 7) error stop

   allocate(h%names(0))
   if (len(h%names) /= 4) error stop
end program string_122
