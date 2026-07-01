module xutil_all_min_mod
implicit none

interface str
   module procedure str_int
end interface

contains

elemental function str_int(i,ndigits,prefix) result(ch)
integer, intent(in) :: i
integer, intent(in), optional :: ndigits
character(len=*), intent(in), optional :: prefix
character(len=32) :: ch
write(ch,"(i0)") i
if (present(prefix)) ch = trim(prefix) // trim(ch)
end function str_int

pure function repeat_char(n,xx) result(yy)
integer, intent(in) :: n
character(len=*), intent(in) :: xx
character(len=len(xx)) :: yy(n)
integer :: i
yy = (/(xx,i=1,n)/)
end function repeat_char

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

end module xutil_all_min_mod

program main
use xutil_all_min_mod
implicit none
character(len=4) :: words(2)
words = [character(len=4) :: "ab", "cd"]
print "(100(a,1x))", repeat_char_vec(2, words)
end program main
