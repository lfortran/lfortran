program character_22
   implicit none
   character(*), parameter :: str = 'This is a string'
   integer :: sig

   sig = sum_string(str)
   print *, sig
   if (sig /= 1484) error stop

contains

   function sum_string(str) result(sig)
      character(len=*), intent(in) :: str
      integer                      :: sig

      character, dimension(len(str)) :: tmp
      integer :: i

      do i = 1, len(str)
         tmp(i) = str(i:i)
      end do

      sig = sum(ichar(tmp))

   end function sum_string

end program character_22
