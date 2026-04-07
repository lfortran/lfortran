module modules_73_mod
   implicit none

contains

   function repeat_char_vec(n,xx,alternate) result(yy)
      integer, intent(in) :: n
      character(len=*), intent(in) :: xx(:)
      logical, intent(in), optional :: alternate
      character(len=len(xx)) :: yy(n*size(xx))
      integer :: i,j
      if (present(alternate)) then
         if (.not. alternate) then
            yy = (/((xx(j),i=1,n),j=1,size(xx))/)
         else
            yy = (/(xx,i=1,n)/)
         end if
      else
         yy = (/(xx,i=1,n)/)
      end if
   end function repeat_char_vec

end module modules_73_mod

program modules_73
   use modules_73_mod
   implicit none
   character(len=4) :: words(2)
   character(len=100) :: result
   words = [character(len=4) :: "ab", "cd"]

   open(unit=10, file="modules_73_data.txt", status="replace")
   write(10, "(100(a,1x))") repeat_char_vec(2, words)
   close(10)

   open(unit=10, file="modules_73_data.txt", status="old")
   read(10, "(100(a,1x))") result
   close(10)

   print *, result
   if (trim(result) /= "ab   cd   ab   cd") error stop
end program modules_73
