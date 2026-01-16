program str_to_char
   implicit none

   character(kind=1, len=3) :: text = "hej"
   character(kind=1, len=*), parameter :: lowercase = "abcdefghijklmnopqrstuvwxyz"
   integer :: it = 1

   call get_index(text(it:))

contains

   subroutine get_index(c)
      character(kind=1), intent(in) :: c
      if (len(c) /= 1) error stop
      if (index(lowercase, c) /= 8) error stop
   end subroutine

end program str_to_char
