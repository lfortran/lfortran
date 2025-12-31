program main
   implicit none

   character(kind=1, len=3) :: text = "hej"
   character(kind=1,len=*), parameter :: lowercase="abcdefghijklmnopqrstuvwxyz"

   integer :: it = 1

   call get_index(text(it:))

contains

   subroutine get_index(c)
      character(kind=1), intent(in) :: c

      print *, "get_index called with: ", len(c)
      print *, "index: ", index(lowercase, c)
   end subroutine
end program