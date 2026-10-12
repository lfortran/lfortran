module intrinsics_len_01_mod
   implicit none
   type :: holder
      character(len=4), allocatable :: names(:)
   end type holder
   type :: item
      character(len=6) :: s
   end type item
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

   integer function len_explicit(n, a) result(r)
      integer, intent(in) :: n
      character(len=*), intent(in) :: a(n)
      r = len(a)
   end function len_explicit

   function pad(a) result(r)
      character(len=*), intent(in) :: a(:)
      character(len=len(a) + 1) :: r
      r = repeat("x", len(r))
   end function pad
end module intrinsics_len_01_mod

program intrinsics_len_01
   use intrinsics_len_01_mod
   implicit none
   character(len=5) :: empty(0)
   character(len=3) :: full(4)
   character(len=6) :: grid(0, 3)
   character(len=:), allocatable :: dal(:)
   type(holder) :: h
   type(item) :: items(3)

   full = "abc"

   if (len_lb0(empty) /= 5) error stop
   if (len_1d(empty) /= 5) error stop
   if (len_1d(full) /= 3) error stop
   if (len_1d(full(3:2)) /= 3) error stop
   if (len_2d(grid) /= 6) error stop
   if (len_explicit(0, empty) /= 5) error stop
   if (len_explicit(4, full) /= 3) error stop
   if (len(pad(empty)) /= 6) error stop
   if (pad(full) /= "xxxx") error stop

   allocate(character(len=7) :: dal(0))
   if (len(dal) /= 7) error stop

   allocate(h%names(0))
   if (len(h%names) /= 4) error stop

   if (len(items%s) /= 6) error stop
   if (len(items(1:2)%s) /= 6) error stop
   if (len(items(3:2)%s) /= 6) error stop

   print *, len_lb0(empty), len_1d(full), len_2d(grid), len(dal)
end program intrinsics_len_01
