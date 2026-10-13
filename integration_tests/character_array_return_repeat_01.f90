module character_array_return_repeat_01_mod
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

subroutine check_values(values, expected)
character(len=*), intent(in) :: values(:), expected(:)
if (size(values) /= 4) error stop "incorrect result size"
if (len(values) /= 4) error stop "incorrect result length"
if (any(values /= expected)) error stop "incorrect result values"
end subroutine check_values

end module character_array_return_repeat_01_mod

program character_array_return_repeat_01
use character_array_return_repeat_01_mod
implicit none
character(len=4) :: words(2)
character(len=40) :: buffer
words = [character(len=4) :: "ab", "cd"]

! CTest checks this direct function-call output, including both markers.
print "(a)", "BEGIN_REPEAT_CHAR_VEC"
print "(100(a,1x))", repeat_char_vec(2, words)

call check_values(repeat_char_vec(2, words), ["ab  ", "cd  ", "ab  ", "cd  "])
call check_values(repeat_char_vec(2, words, .true.), ["ab  ", "cd  ", "ab  ", "cd  "])
call check_values(repeat_char_vec(2, words, .false.), ["ab  ", "ab  ", "cd  ", "cd  "])

write(buffer, "(100(a,1x))") repeat_char_vec(2, words)
if (buffer /= "ab   cd   ab   cd") error stop "incorrect default formatted values"
write(buffer, "(100(a,1x))") repeat_char_vec(2, words, .true.)
if (buffer /= "ab   cd   ab   cd") error stop "incorrect alternating formatted values"
write(buffer, "(100(a,1x))") repeat_char_vec(2, words, .false.)
if (buffer /= "ab   ab   cd   cd") error stop "incorrect grouped formatted values"

! Emit completion only after every value and formatting assertion passed.
print "(a)", "END_REPEAT_CHAR_VEC"
end program character_array_return_repeat_01
