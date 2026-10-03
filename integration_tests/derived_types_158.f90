module m_constants
implicit none
private

character(len=*),parameter      :: letters(4)=['a','b','c','d']

type calendar
   character(len=len(letters))  :: chars(4)=letters
end type calendar
type(calendar),public,parameter :: calen=calendar( )

end module m_constants
program testit
use M_constants, only : calen
if (any(calen%chars /= ['a','b','c','d'])) then
    error stop
end if
end program testit