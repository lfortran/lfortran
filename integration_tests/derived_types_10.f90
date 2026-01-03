program debug
implicit none
type :: char_struct

   !> Newline character
   character(len=1) :: f = achar(70)

   !> Tabulators are allowed as whitespace and in strings
   character(len=1) :: g = achar(71)

end type char_struct

type(char_struct) :: alphabets

if( alphabets%f /= 'F' ) error stop
if( alphabets%g /= 'G' ) error stop
print *, alphabets%f, alphabets%g

alphabets%f = achar(80)
alphabets%g = achar(81)
if( alphabets%f /= 'P' ) error stop
if( alphabets%g /= 'Q' ) error stop
print *, alphabets%f, alphabets%g

end program debug
