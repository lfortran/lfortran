program optional_string_arg_01
   implicit none
   character(10) :: a

   ! Test absent case
   call check(1)
   
   ! Test present case
   call check(1, 'a')

   ! Test caller-unchanged check
   a = "1234567890"
   call sub(3, a)
   if (len(a) /= 10) error stop
   if (a /= "1234567890") error stop

contains
   subroutine check(n, s)
      integer, intent(in) :: n
      character(len=n), optional, intent(in) :: s
      if (len(s) /= 1) error stop
      if (present(s)) then
         if (s /= 'a') error stop
      end if
   end subroutine

   subroutine sub(n, s)
      integer, intent(in) :: n
      character(len=n), optional, intent(in) :: s
      if (len(s) /= 3) error stop
   end subroutine
end program optional_string_arg_01
