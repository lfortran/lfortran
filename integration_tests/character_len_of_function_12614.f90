module m_system
   implicit none
contains

   character(len=28) function myfunc(iarg)
      integer,intent(in),optional :: iarg
      if(present(iarg))then
         myfunc=repeat('@',iarg)
      else
         myfunc='fini '//printdat()
      end if
   contains

      function printdat()
         character(len=len(myfunc)) :: printdat
         printdat='printed'
      end function printdat

   end function myfunc

end module m_system
program testit
use m_system, only : myfunc
   write(*,*)myfunc(),myfunc(3)
end program testit
